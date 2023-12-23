(* to use results of parsing *)
open Typedtree

(* Yul and ABI *)
open Yul_ast
open Abi_json

exception Not_implemented
exception Undefined_storage
exception Not_allowed_function

(* names of default runtime *)
let runtime = Strlit "runtime"

(* deploy code part *)
let deploy_code =
  Code
    [
      Exp (EVM (Sstore (Literal (Dec 0), EVM Caller)));
      Exp
        (EVM
           (Datacopy
              (Literal (Dec 0), EVM (Dataoffset runtime), EVM (Datasize runtime))));
      Exp (EVM (Return (Literal (Dec 0), EVM (Datasize runtime))));
    ]

(* names of default functions *)
let return_uint = "returnUint"
let return_true = "returnTrue"
let get_storage = "getStorage"
let set_storage = "setStorage"
let get_hash_slot = "getHashSlot"
let selector = "selector"
let decode_as_uint = "decodeAsUint"
let decode_as_address = "decodeAsAddress"

(* definitions of default functions *)
let gen_return_uint_name n = "returnUint_" ^ string_of_int n

let return_uint_def n =
  let rec gen_arg_vars n =
    if n = 0 then []
    else ("arg_" ^ string_of_int (n - 1), n - 1) :: gen_arg_vars (n - 1)
  in
  let num_args = List.rev (gen_arg_vars n) in
  let rec gen_store_vars args =
    match args with
    | [] -> []
    | (x, n) :: tl ->
        Exp (EVM (Mstore (Literal (Dec (n * 32)), ID x))) :: gen_store_vars tl
  in
  let args = fst (List.split num_args) in
  let assigns =
    gen_store_vars num_args
    @ [ Exp (EVM (Return (Literal (Dec 0), Literal (Hex (32 * n))))) ]
  in
  FunctionDef (gen_return_uint_name n, args, [], assigns)

let return_true_def =
  FunctionDef
    ( return_true,
      [],
      [],
      [
        Exp (EVM (Mstore (Literal (Dec 0), Literal (Dec 1))));
        Exp (EVM (Return (Literal (Dec 0), Literal (Hex 32))));
      ] )

let get_storage_def n =
  let rec gen_ret_vars n =
    if n = 0 then []
    else ("ret_" ^ string_of_int (n - 1), n) :: gen_ret_vars (n - 1)
  in
  let ret_num_args = List.rev (gen_ret_vars n) in
  let rec gen_assign_vars args =
    match args with
    | [] -> []
    | (x, n) :: tl ->
        Assign ((x, []), EVM (Sload (Literal (Dec n)))) :: gen_assign_vars tl
  in
  let ret_args = fst (List.split ret_num_args) in
  let assigns = gen_assign_vars ret_num_args in
  FunctionDef (get_storage, [], ret_args, assigns)

let set_storage_def n =
  let rec gen_arg_vars n =
    if n = 0 then []
    else ("arg_" ^ string_of_int (n - 1), n) :: gen_arg_vars (n - 1)
  in
  let num_args = List.rev (gen_arg_vars n) in
  let rec gen_store_vars args =
    match args with
    | [] -> []
    | (x, n) :: tl ->
        Exp (EVM (Sstore (Literal (Dec n), ID x))) :: gen_store_vars tl
  in
  let args = fst (List.split num_args) in
  let assigns = gen_store_vars num_args in
  FunctionDef (set_storage, args, [], assigns)

let get_hash_slot_def =
  let slot = "$slot" in
  let key = "$key" in
  let data_slot = "$dataSlot" in
  FunctionDef
    ( get_hash_slot,
      [ slot; key ],
      [ data_slot ],
      [
        Exp (EVM (Mstore (Literal (Hex 0), ID key)));
        Exp (EVM (Mstore (Literal (Hex 32), ID slot)));
        Assign
          ((data_slot, []), EVM (Keccak256 (Literal (Hex 0), Literal (Hex 64))));
      ] )

let selector_def =
  let return_arg = "ret" in
  FunctionDef
    ( selector,
      [],
      [ return_arg ],
      [
        Assign
          ( (return_arg, []),
            EVM (Shr (Literal (Dec 224), EVM (Calldataload (Literal (Dec 0)))))
          );
      ] )

let decode_as_uint_def =
  let arg = "offset" in
  let return_arg = "v" in
  let pos = "pos" in
  FunctionDef
    ( decode_as_uint,
      [ arg ],
      [ return_arg ],
      [
        Let
          ( (pos, []),
            EVM (Add (Literal (Dec 4), EVM (Mul (ID arg, Literal (Hex 32))))) );
        Assign ((return_arg, []), EVM (Calldataload (ID pos)));
      ] )

let default_revert_def =
  Default [ Exp (EVM (Revert (Literal (Dec 0), Literal (Dec 0)))) ]

let default_function_defs = [ selector_def; decode_as_uint_def ]

(* splitting signatures to the types part and vals part *)
let rec split_module_sig = function
  | [] -> ([], [])
  | x :: xs -> (
      let types, vals = split_module_sig xs in
      match x.sig_desc with
      | Tsig_type (_, y) -> (y @ types, vals)
      | Tsig_value y -> (types, y :: vals)
      | _ -> raise Not_implemented)

(* name of storage type *)
(* for now, storage type must be int *)
let get_storage_name = function
  | [ { typ_id = s; _ } ] -> [ Some s; None ]
  | [ { typ_id = s1; _ }; { typ_id = s2; _ } ] -> [ Some s1; Some s2 ]
  | _ -> raise Not_implemented

let get_storage_num =
  let rec get_storage_num_aux { ctyp_desc = t; _ } =
    match t with
    | Ttyp_constr (p, _, _) -> (
        match p with
        | Pident id -> if Ident.name id = "unit" then 0 else 1
        | _ -> 1)
    | Ttyp_tuple types ->
        List.fold_left (fun acc x -> get_storage_num_aux x + acc) 0 types
    | _ -> assert false
  in
  function
  | [ { typ_manifest = Some t; _ } ] -> (get_storage_num_aux t, 0)
  | [ { typ_manifest = Some t1; _ }; { typ_manifest = Some t2; _ } ] ->
      (get_storage_num_aux t1, get_storage_num_aux t2)
  | _ -> assert false

let abi_input_of_typ typ =
  let abi_of_constr = function
    | Ttyp_constr (Path.Pident s, _, _) ->
        let s = Ident.name s in
        if s = "int" then [ { input_name = ""; input_type = Uint256 } ]
        else if s = "unit" then []
        else raise Not_implemented
    | _ -> raise Not_implemented
  in
  match typ with
  | Ttyp_constr _ -> abi_of_constr typ
  | Ttyp_tuple ctyp_list ->
      List.fold_left
        (fun acc { ctyp_desc = x; _ } -> abi_of_constr x @ acc)
        [] ctyp_list
  | _ -> raise Not_implemented

let abi_output_of_typ typ =
  let abi_of_constr = function
    | Ttyp_constr (Path.Pident s, _, _) ->
        let s = Ident.name s in
        if s = "int" then [ { output_name = ""; output_type = Uint256 } ]
        else if s = "unit" then []
        else raise Not_implemented
    | _ -> raise Not_implemented
  in
  match typ with
  | Ttyp_constr _ -> abi_of_constr typ
  | Ttyp_tuple ctyp_list ->
      List.fold_left
        (fun acc { ctyp_desc = x; _ } -> abi_of_constr x @ acc)
        [] ctyp_list
  | _ -> raise Not_implemented

let keccak_256_for_abi func_sig =
  let open Digestif in
  Hex
    (int_of_string
       ("0x"
       ^ String.sub
           (KECCAK_256.to_hex
              (KECCAK_256.get
                 (KECCAK_256.feed_string KECCAK_256.empty func_sig)))
           0 8))

let params_in_case inputs =
  let decoder = function
    | Uint256 -> decode_as_uint
    | Address -> decode_as_address
  in
  let rec aux n = function
    | [] -> []
    | { input_type = x; _ } :: xs ->
        FunctionCall (decoder x, [ Literal (Dec n) ]) :: aux (n + 1) xs
  in
  aux 0 inputs

let returner_in_case = function
  | [] -> (return_true, return_true_def)
  | x ->
      let n = List.length x in
      (gen_return_uint_name n, return_uint_def n)

let rec uncurry_sig = function
  | Ttyp_arrow (_, { ctyp_desc = arg; _ }, { ctyp_desc = body; _ }) ->
      let args, ret = uncurry_sig body in
      (arg :: args, ret)
  | Ttyp_tuple [ { ctyp_desc = p1; _ }; { ctyp_desc = p2; _ } ] -> ([], (p1, p2))
  | _ -> raise Not_implemented

(* accesible functions must be the form "params -> storage -> (return val, storage)" *)
(* generating (ABI, (func_name, keccak256 of a function signature)) from a signature  *)
let abi_of_sig = function
  | [ Some stor; mstor ] -> (
      function
      | { val_id = name; val_desc = { ctyp_desc = core_desc; _ }; _ } ->
          let args, ret, s2 =
            match uncurry_sig core_desc with
            | args, (ret, Ttyp_constr (Path.Pident s2, _, _)) -> (args, ret, s2)
            | _ -> raise Not_allowed_function
          in
          let arg, s1, ms =
            match args with
            | [ arg; Ttyp_constr (Path.Pident s1, _, _) ] -> (arg, s1, None)
            | [
             arg;
             Ttyp_constr (Path.Pident s1, _, _);
             Ttyp_constr (Path.Pident ms, _, _);
            ] ->
                (arg, s1, Some ms)
            | _ -> raise Not_allowed_function
          in
          if
            (match (ms, mstor) with
            | Some ms', Some mstor' -> Ident.same ms' mstor'
            | None, _ -> true
            | _, _ -> assert false)
            && Ident.same stor s1 && Ident.same stor s2
          then
            let func_name = Ident.name name in
            let inputs = abi_input_of_typ arg in
            let outputs = abi_output_of_typ ret in
            let abi =
              {
                abi_name = func_name;
                abi_type = Func;
                abi_inputs = inputs;
                abi_outputs = outputs;
                abi_mutability = Nonpayable;
              }
            in
            let return_func, added_func = returner_in_case outputs in
            ( (abi, added_func),
              ( func_name,
                keccak_256_for_abi (signature_of_function abi),
                params_in_case inputs,
                return_func,
                List.length outputs ) )
          else raise Not_implemented)
  | _ -> raise Not_allowed_function

(* translate functions *)
let translate_function func_name e =
  let rec expand_function e =
    match e with
    | {
     exp_desc =
       Texp_function { cases = { c_lhs = arg_pat; c_rhs = body; _ } :: []; _ };
     _;
    } ->
        let first_rename, first_args = Utils.flatten_tuple_pat arg_pat in
        let rename, args, body = expand_function body in
        (first_rename @ rename, first_args @ args, body)
    | _ -> ([], [], e)
  in
  let rename, args, body = expand_function e in
  let open Anormal in
  let aval_to_yul = function
    | Var s -> ID s
    | IntV n -> Literal (Dec n)
    | BoolV b -> if b then Literal (Dec 1) else Literal (Dec 0)
    | StrV s -> Literal (Str (Strlit s))
    | _ -> assert false
  in
  let translate_aval_args v =
    match v with UnitV -> None | _ -> Some (aval_to_yul v)
  in
  let letexp_to_yul = function
    | LVal v -> aval_to_yul v
    | LApp (Var s, vals) ->
        FunctionCall
          ( s,
            List.fold_left
              (fun acc x ->
                match translate_aval_args x with
                | Some v -> v :: acc
                | None -> acc)
              [] vals
            |> List.rev )
    | LApp (Bop b, [ v1; v2 ]) ->
        let v1 = aval_to_yul v1 in
        let v2 = aval_to_yul v2 in
        EVM
          (match b with
          | Add -> Add (v1, v2)
          | Sub -> Sub (v1, v2)
          | Mul -> Mul (v1, v2)
          | Div -> Div (v1, v2))
    | LApp (HashAdd, [ h; x; y ]) ->
        EVM
          (Sstore
             ( FunctionCall (get_hash_slot, [ aval_to_yul h; aval_to_yul x ]),
               aval_to_yul y ))
    | LApp (HashFind, [ h; x ]) ->
        EVM
          (Sload
             (FunctionCall (get_hash_slot, [ aval_to_yul h; aval_to_yul x ])))
    | _ -> assert false
  in
  let return_exp vals acc =
    let vals = List.filter (fun x -> not (x = UnitV)) vals in
    let rec assign_rets vals acc_exp acc_rets =
      match vals with
      | [] -> (acc_exp, acc_rets)
      | v :: vs ->
          let ret = Utils.fresh_var () in
          assign_rets vs
            (Assign ((ret, []), aval_to_yul v) :: acc_exp)
            (ret :: acc_rets)
    in
    assign_rets vals acc []
  in
  let rec translate_body_aux e acc =
    match e with
    | Rexp e' -> (
        match e' with
        | RTuple vals -> return_exp vals acc
        | RVal v -> return_exp [ v ] acc)
    | Seq (e1, e2) ->
        let acc = Exp (letexp_to_yul e1) :: acc in
        translate_body_aux e2 acc
    | Letin (vars, e1, e2) ->
        let acc = Let ((List.hd vars, List.tl vars), letexp_to_yul e1) :: acc in
        translate_body_aux e2 acc
  in
  let statements, return_vars = translate_body_aux (normalize body rename) [] in
  let statements = List.rev statements in
  let return_vars = List.rev return_vars in
  FunctionDef (func_name, args, return_vars, statements)

let translate_external_func func_sigs storage_num mut_storage_num = function
  | { vb_pat = { pat_desc = Tpat_var (func_name, _); _ }; vb_expr = e; _ } :: []
    -> (
      let func_uniq_name = Ident.unique_name func_name in
      ( translate_function func_uniq_name e,
        (* if the function is public, then a switch statement is generated. *)
        match
          List.find_opt
            (fun (x, _, _, _, _) -> x = Ident.name func_name)
            func_sigs
        with
        | Some (_, sig_string, params, returner, return_num) ->
            let set_stor_vals, returned_vals, returned_stor_vals, mut_stor_vals
                =
              let rec gen_return_vals n =
                if n = 0 then []
                else Utils.fresh_var () :: gen_return_vals (n - 1)
              in
              ( List.rev (gen_return_vals storage_num),
                List.rev (gen_return_vals return_num),
                List.rev (gen_return_vals storage_num),
                List.init mut_storage_num (fun x ->
                    Literal (Hex (x + storage_num + 1))) )
            in
            let call_get_storage, call_set_storage =
              match (set_stor_vals, returned_stor_vals) with
              | [], [] -> (None, None)
              | _ :: _, _ :: _ ->
                  ( Some
                      (Let
                         ( (List.hd set_stor_vals, List.tl set_stor_vals),
                           FunctionCall (get_storage, []) )),
                    Some
                      (Exp
                         (FunctionCall
                            ( set_storage,
                              List.map (fun x -> ID x) returned_stor_vals ))) )
              | _, _ -> assert false
            in
            let result_vals = returned_vals @ returned_stor_vals in
            let call_body_function =
              match result_vals with
              | [] ->
                  Some
                    (Exp
                       (FunctionCall
                          ( func_uniq_name,
                            params
                            @ List.map (fun x -> ID x) set_stor_vals
                            @ mut_stor_vals )))
              | _ ->
                  Some
                    (Let
                       ( (List.hd result_vals, List.tl result_vals),
                         FunctionCall
                           ( func_uniq_name,
                             params
                             @ List.map (fun x -> ID x) set_stor_vals
                             @ mut_stor_vals ) ))
            in
            let case_result =
              List.filter_map
                (fun x -> x)
                [
                  call_get_storage;
                  call_body_function;
                  call_set_storage;
                  Some
                    (Exp
                       (FunctionCall
                          (returner, List.map (fun x -> ID x) returned_vals)));
                ]
            in
            Some (Case (sig_string, case_result))
        | None -> None ))
  | _ -> assert false

(* splitting module structure to types and external vals and internal vals *)
let rec split_module_str = function
  | [] -> ([], [])
  | x :: xs -> (
      let types, exps = split_module_str xs in
      match x.str_desc with
      | Tstr_type (_, y) -> (y @ types, exps)
      | Tstr_value (_, y) -> (types, y :: exps)
      | _ -> raise Not_implemented)

let backend source_file Typedtree.{ structure; _ } =
  match structure with
  | {
   str_items =
     [
       {
         str_desc =
           Tstr_module
             {
               mb_id = Some contract_name;
               mb_expr =
                 {
                   mod_desc =
                     Tmod_constraint
                       ( {
                           mod_desc = Tmod_structure { str_items = items; _ };
                           _;
                         },
                         _,
                         Tmodtype_explicit
                           {
                             mty_desc = Tmty_signature { sig_items = sigs; _ };
                             _;
                           },
                         _ );
                   _;
                 };
               _;
             };
         _;
       };
     ];
   _;
  } ->
      let contract_name = Ident.name contract_name in
      (* getting storage id and signatures *)
      let stor, sigs = split_module_sig sigs in
      let stor_name = get_storage_name stor in
      (* getting (ABI and newly required return functions) and function signatures *)
      let abis_and_funcs, func_sigs =
        List.split (List.map (abi_of_sig stor_name) sigs)
      in
      let abis, default_funcs = List.split abis_and_funcs in
      let default_function_defs =
        List.fold_left
          (fun acc x -> if List.mem x acc then acc else x :: acc)
          default_function_defs default_funcs
      in
      (* getting val expression (types are not yet implemented) *)
      let types, exps = split_module_str items in
      (* getting the number of storage *)
      let storage_num, mut_storage_num = get_storage_num types in
      (* getting function declarations and dispatcher cases *)
      let funcs, cases =
        List.split
          (List.map
             (translate_external_func func_sigs storage_num mut_storage_num)
             exps)
      in
      (* generating and adding set/get storage functions *)
      let default_function_defs =
        let storage_operations =
          if storage_num > 0 then
            [ get_storage_def storage_num; set_storage_def storage_num ]
          else []
        in
        let mut_storage_operations =
          if mut_storage_num > 0 then [ get_hash_slot_def ] else []
        in
        storage_operations @ mut_storage_operations @ default_function_defs
      in
      (* code fragment: dispatcher *)
      let dispatcher =
        let cases = List.filter_map (fun x -> x) cases in
        Switch (FunctionCall (selector, []), cases, default_revert_def)
      in
      (* generating whole Yul code *)
      let yul_code =
        Object
          ( Strlit contract_name,
            deploy_code,
            Some
              (Object
                 ( runtime,
                   Code ((dispatcher :: funcs) @ default_function_defs),
                   None )) )
      in
      (* print debugging *)
      (* print_endline (string_of_yul yul_code) *)
      (* json provided for solc *)
      let yul_code = json_string_of_yul yul_code in
      let yul_json : Yojson.Basic.t =
        `Assoc
          [
            ("language", `String "Yul");
            ( "sources",
              `Assoc
                [ ("destructible", `Assoc [ ("content", `String yul_code) ]) ]
            );
            ( "settings",
              `Assoc
                [
                  ( "outputSelection",
                    `Assoc
                      [
                        ( "*",
                          `Assoc
                            [
                              ( "*",
                                `List
                                  [
                                    `String "evm.bytecode.object";
                                    `String "evm.deployedBytecode.object";
                                  ] );
                            ] );
                      ] );
                ] );
          ]
      in
      (* solc compilation result *)
      let tmp_json_name = "___" ^ contract_name ^ ".json" in
      Yojson.Basic.to_file tmp_json_name yul_json;
      let compiled_json =
        Unix.open_process_in ("solc --standard-json " ^ tmp_json_name)
        |> Yojson.Basic.from_channel
      in
      Sys.remove tmp_json_name;
      (* If solc causes some errors, solc writes a log into a .json file.
         Thus, to see solc error output, the line below is helpful. *)
      (* Yojson.Basic.pretty_print Format.std_formatter compiled_json; *)
      (* getting required values *)
      let evm_field =
        compiled_json
        |> Yojson.Basic.Util.member "contracts"
        |> Yojson.Basic.Util.member "destructible"
        |> Yojson.Basic.Util.member contract_name
        |> Yojson.Basic.Util.member "evm"
      in
      let bytecode : Yojson.Basic.t =
        `Assoc
          [
            ( "bytecode",
              evm_field
              |> Yojson.Basic.Util.member "bytecode"
              |> Yojson.Basic.Util.member "object" );
          ]
      in
      let deployed_bytecode : Yojson.Basic.t =
        `Assoc
          [
            ( "deployedBytecode",
              evm_field
              |> Yojson.Basic.Util.member "deployedBytecode"
              |> Yojson.Basic.Util.member "object" );
          ]
      in
      (* combining ABI and solc compilation result  *)
      let result_json =
        List.fold_left Yojson.Basic.Util.combine
          (`Assoc [ ("contractName", `String contract_name) ])
          [ json_of_abis abis; bytecode; deployed_bytecode ]
        |> Yojson.Basic.pretty_to_string
      in
      let result_path =
        "./" :: String.split_on_char '/' source_file
        |> List.rev |> List.tl |> List.tl |> List.rev |> String.concat "/"
        |> fun path -> path ^ "/contracts"
      in
      let contract_path = result_path ^ "/" ^ contract_name ^ ".json" in
      let result_chan =
        try
          Unix.mkdir result_path 0o775;
          open_out contract_path
        with Unix.Unix_error _ -> open_out contract_path
      in
      output_string result_chan result_json;
      close_out result_chan
  | _ -> raise Not_implemented
