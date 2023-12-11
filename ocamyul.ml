(* to use results of parsing *)
open Typedtree

(* open Asttypes *)
(* open Path *)

(* for keccak256 *)
open Digestif

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

(* let return_uint_def =
   let arg = "v" in
   FunctionDef
     ( return_uint,
       [ arg ],
       [],
       [
         Exp (EVM (Mstore (Literal (Dec 0), ID arg)));
         Exp (EVM (Return (Literal (Dec 0), Literal (Hex 32))));
       ] ) *)

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

(* let get_storage_def =
     let return_arg = "ret" in
     FunctionDef
       ( get_storage,
         [],
         [ return_arg ],
         [ Assign ((return_arg, []), EVM (Sload (Literal (Dec 1)))) ] )

   let set_storage_def =
     let arg = "v" in
     FunctionDef
       (set_storage, [ arg ], [], [ Exp (EVM (Sstore (Literal (Dec 1), ID arg))) ]) *)

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

let default_function_defs =
  [
    (* return_uint_def; *)
    (* return_true_def; *)
    (* get_storage_def; *)
    (* set_storage_def; *)
    selector_def;
    decode_as_uint_def;
  ]

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
let storage_name = function
  | [ { typ_id = s; _ } ] -> s
  | _ -> raise Not_implemented

let get_storage_num = function
  | [ { typ_manifest = Some t; _ } ] ->
      let rec get_storage_num_aux { ctyp_desc = t; _ } =
        match t with
        | Ttyp_constr _ -> 1
        | Ttyp_tuple types ->
            List.fold_left (fun acc x -> get_storage_num_aux x + acc) 0 types
        | _ -> assert false
      in
      get_storage_num_aux t
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
  | [] -> (None, return_true_def)
  | x ->
      let n = List.length x in
      (Some (gen_return_uint_name n), return_uint_def n)

(* | [ { output_type = x; _ } ] -> (
    match x with
    | Uint256 -> Some return_uint
    | Address -> raise Not_implemented) *)
(* | _ -> raise Not_implemented *)

(* accesible functions must be the form "params -> storage -> (return val, storage)" *)
(* generating (ABI, (func_name, keccak256 of a function signature)) from a signature  *)
let abi_of_sig stor = function
  | { val_id = name; val_desc = { ctyp_desc = core_desc; _ }; _ } -> (
      match core_desc with
      | Ttyp_arrow
          ( _,
            { ctyp_desc = arg; _ },
            {
              ctyp_desc =
                Ttyp_arrow
                  ( _,
                    { ctyp_desc = Ttyp_constr (Path.Pident s1, _, _); _ },
                    {
                      ctyp_desc =
                        Ttyp_tuple
                          [
                            { ctyp_desc = ret; _ };
                            {
                              ctyp_desc = Ttyp_constr (Path.Pident s2, _, _);
                              _;
                            };
                          ];
                      _;
                    } );
              _;
            } ) ->
          if Ident.same stor s1 && Ident.same stor s2 then
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
          else raise Not_implemented
      | _ -> raise Not_allowed_function)

(* translate public (belonging to sig...end) function *)
let translate_public ast return_args rename =
  let open Anormal in
  let aval_to_yul = function
    | Var s -> ID s
    | IntV n -> Literal (Dec n)
    | BoolV b -> if b then Literal (Dec 1) else Literal (Dec 0)
    | StrV s -> Literal (Str (Strlit s))
    | UnitV -> assert false
  in
  let fexp_to_yul = function
    | Val v -> aval_to_yul v
    | Bop (v1, b, v2) ->
        let v1 = aval_to_yul v1 in
        let v2 = aval_to_yul v2 in
        EVM
          (match b with
          | Add -> Add (v1, v2)
          | Sub -> Sub (v1, v2)
          | Mul -> Mul (v1, v2)
          | Div -> Div (v1, v2))
    | App (Var s, vals) -> FunctionCall (s, List.map aval_to_yul vals)
    | _ -> assert false
  in
  (* let acexp_to_yul = function Fexp e -> fexp_to_yul e | _ -> assert false in *)
  let return_exp vals acc =
    let vals = List.filter (fun x -> not (x = UnitV)) vals in
    (* splitting return values and storage values *)
    let rec split_tuple n rets stors =
      if n = 0 then (List.rev rets, stors)
      else split_tuple (n - 1) (List.hd stors :: rets) (List.tl stors)
    in
    let rets, stors = split_tuple (List.length return_args) [] vals in
    let acc =
      Exp (FunctionCall (set_storage, List.map aval_to_yul stors)) :: acc
    in
    let rec assign_rets rets_and_vals acc =
      match rets_and_vals with
      | [], [] -> acc
      | x :: xs, v :: vs ->
          assign_rets (xs, vs) (Assign ((x, []), aval_to_yul v) :: acc)
      | _ -> assert false
    in
    assign_rets (return_args, rets) [] @ acc
  in
  let rec translate_body_aux e acc =
    match e with
    | Cexp e' -> (
        match (e', acc) with
        | Tuple vals, [] -> return_exp vals []
        | _ -> assert false)
    | Letin (vars, e1, e2) -> (
        let acc = Let ((List.hd vars, List.tl vars), fexp_to_yul e1) :: acc in
        match e2 with
        | Cexp (Tuple vals) -> return_exp vals acc
        | Letin _ -> translate_body_aux e2 acc
        | _ -> assert false)
  in
  (* List.rev (translate_body_aux (normalize ast) []) *)
  List.rev (translate_body_aux (normalize ast rename) [])

(* flatten arguments tuple pattern *)
let flatten_tuple_pat p =
  let open Types in
  let rec count_tuple_elem = function
    | Tconstr _ -> 1
    | Ttuple types ->
        List.fold_left
          (fun acc x -> count_tuple_elem x + acc)
          0 (List.map get_desc types)
    | Tvar _ -> 1
    | _ -> assert false
  in
  let rec gen_renaming_ids n =
    if n = 0 then []
    else
      let x = Anormal.fresh_var () in
      x :: gen_renaming_ids (n - 1)
  in
  let rec flatten_tuple_pat_aux (p, t) =
    match (p, get_desc t) with
    | Tpat_any, _ -> ([], [ Anormal.fresh_var () ])
    | ( Tpat_construct (_, { Types.cstr_name = "()"; _ }, [], _),
        Tconstr (_, [], _) ) ->
        ([], [])
    | Tpat_var (s, _), t ->
        let n = count_tuple_elem t in
        if n > 1 then
          let renamed = gen_renaming_ids n in
          ([ (Ident.unique_name s, renamed) ], renamed)
        else ([], [ Ident.unique_name s ])
    | Tpat_tuple pat_list, Ttuple type_list ->
        List.fold_left
          (fun acc (x, y) ->
            let renaming, generated_ids =
              flatten_tuple_pat_aux (x.pat_desc, y)
            in
            (fst acc @ renaming, snd acc @ generated_ids))
          ([], [])
          (List.combine pat_list type_list)
    | _ -> assert false
  in
  flatten_tuple_pat_aux (p.pat_desc, p.pat_type)

(* splitting module structure to types and external vals and internal vals *)
let rec split_module_str = function
  | [] -> ([], [])
  | x :: xs -> (
      let types, exps = split_module_str xs in
      match x.str_desc with
      | Tstr_type (_, y) -> (y @ types, exps)
      | Tstr_value (_, y) -> (types, y :: exps)
      | _ -> raise Not_implemented)

let translate_external_func func_sigs = function
  | {
      vb_pat = { pat_desc = Tpat_var (func_name, _); _ };
      vb_expr = { exp_desc = e; _ };
      _;
    }
    :: [] -> (
      match
        List.find_opt
          (fun (x, _, _, _, _) -> x = Ident.name func_name)
          func_sigs
      with
      | Some (_, sig_string, params, returner, return_num) -> (
          match e with
          | Texp_function
              {
                cases =
                  {
                    c_lhs = arg_pat;
                    c_rhs =
                      {
                        exp_desc =
                          Texp_function
                            {
                              cases =
                                { c_lhs = stor_pat; c_rhs = body; _ } :: [];
                              _;
                            };
                        _;
                      };
                    _;
                  }
                  :: [];
                _;
              } ->
              let func_name = Ident.unique_name func_name in
              let input_renaming, input_args = flatten_tuple_pat arg_pat in
              let storage_renaming, storage_args = flatten_tuple_pat stor_pat in
              let return_args =
                let rec gen_return_args n =
                  if n = 0 then []
                  else ("$ret_" ^ string_of_int n) :: gen_return_args (n - 1)
                in
                List.rev (gen_return_args return_num)
              in
              let return_vals =
                let rec gen_return_vals n =
                  if n = 0 then []
                  else Anormal.fresh_var () :: gen_return_vals (n - 1)
                in
                List.rev (gen_return_vals return_num)
              in
              let case_result =
                match returner with
                | Some return_func ->
                    [
                      Let
                        ( (List.hd return_vals, List.tl return_vals),
                          FunctionCall (func_name, params) );
                      Exp
                        (FunctionCall
                           (return_func, List.map (fun x -> ID x) return_vals));
                    ]
                | None ->
                    [
                      Exp (FunctionCall (func_name, params));
                      Exp (FunctionCall (return_true, []));
                    ]
              in
              ( FunctionDef
                  ( func_name,
                    input_args,
                    return_args,
                    Let
                      ( (List.hd storage_args, List.tl storage_args),
                        FunctionCall (get_storage, []) )
                    :: translate_public body return_args
                         (input_renaming @ storage_renaming) ),
                Some (Case (sig_string, case_result)) )
          | _ -> raise Not_implemented)
      | None -> raise Not_implemented)
  | _ -> assert false

let backend _ Typedtree.{ structure; _ } =
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
      let stor_name = storage_name stor in
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
      (* getting function declarations and dispatcher cases *)
      let funcs, cases =
        List.split (List.map (translate_external_func func_sigs) exps)
      in
      (* getting the number of storage *)
      let stor_num = get_storage_num types in
      (* generating and adding set/get storage functions *)
      let default_function_defs =
        get_storage_def stor_num :: set_storage_def stor_num
        :: default_function_defs
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
      let result_chan =
        try
          Unix.mkdir "./contracts" 0o775;
          open_out ("./contracts/" ^ contract_name ^ ".json")
        with Unix.Unix_error _ ->
          open_out ("./contracts/" ^ contract_name ^ ".json")
      in
      output_string result_chan result_json;
      close_out result_chan
  | _ -> raise Not_implemented
