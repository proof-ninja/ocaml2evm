(* to use results of parsing *)
open Typedtree

(* Yul and ABI *)
open Yul_ast
open Abi

type function_signature = {
  arg_type : core_type_desc;
  storage_id : Ident.t;
  mut_storage_id : Ident.t option;
  return_type : core_type_desc;
  return_storage_id : Ident.t;
}

exception Not_implemented
exception Not_allowed_function

(* splitting signatures to the types part and vals part *)
let rec split_module_sig = function
  | [] -> ([], [])
  | x :: xs -> (
      let types, vals = split_module_sig xs in
      match x.sig_desc with
      | Tsig_type (_, y) -> (y @ types, vals)
      | Tsig_value y -> (types, y :: vals)
      | Tsig_open _ -> (types, vals)
      | _ -> raise Not_implemented)

(* getting the number of elements in storage and mutable storage *)
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
        if s = "unit" then [] else raise Not_implemented
    | Ttyp_constr (Path.Pdot (_, s), _, _) ->
        if s = "uint" then [ { input_name = ""; input_type = Uint256 } ]
        else if s = "sint" then [ { input_name = ""; input_type = Int256 } ]
        else if s = "address" then [ { input_name = ""; input_type = Address } ]
        else raise Not_implemented
    | _ -> raise Not_implemented
  in
  match typ with
  | Ttyp_constr _ -> abi_of_constr typ
  | Ttyp_tuple ctyp_list ->
      List.fold_left
        (fun acc { ctyp_desc = x; _ } -> acc @ abi_of_constr x)
        [] ctyp_list
  | _ -> raise Not_implemented

let abi_output_of_typ typ =
  let abi_of_constr = function
    | Ttyp_constr (Path.Pident s, _, _) ->
        let s = Ident.name s in
        if s = "unit" then [] else raise Not_implemented
    | Ttyp_constr (Path.Pdot (_, s), _, _) ->
        if s = "uint" then [ { output_name = ""; output_type = Uint256 } ]
        else if s = "sint" then [ { output_name = ""; output_type = Int256 } ]
        else if s = "address" then
          [ { output_name = ""; output_type = Address } ]
        else raise Not_implemented
    | _ -> raise Not_implemented
  in
  match typ with
  | Ttyp_constr _ -> abi_of_constr typ
  | Ttyp_tuple ctyp_list ->
      List.fold_left
        (fun acc { ctyp_desc = x; _ } -> acc @ abi_of_constr x)
        [] ctyp_list
  | _ -> raise Not_implemented

(* the result of applying keccak256 to a function signature *)
let keccak_256_for_abi func_sig =
  let open Digestif in
  (* Hex
     (int_of_string
        ("0x"
        ^ String.sub
            (KECCAK_256.to_hex
               (KECCAK_256.get
                  (KECCAK_256.feed_string KECCAK_256.empty func_sig)))
            0 8)) *)
  Hex
    (String.sub
       (KECCAK_256.to_hex
          (KECCAK_256.get (KECCAK_256.feed_string KECCAK_256.empty func_sig)))
       0 8)

(* the type of function to function_signature type *)
let uncurry_sig t =
  let rec aux t =
    match t with
    | Ttyp_arrow (_, { ctyp_desc = arg; _ }, { ctyp_desc = body; _ }) ->
        let args, ret = aux body in
        (arg :: args, ret)
    | Ttyp_tuple [ { ctyp_desc = p1; _ }; { ctyp_desc = p2; _ } ] ->
        ([], (p1, p2))
    | _ -> raise Not_implemented
  in
  match aux t with
  | ( arg :: Ttyp_constr (Path.Pident s1, _, _) :: x,
      (ret, Ttyp_constr (Path.Pident s2, _, _)) ) ->
      (* The argument of mutable storage can be omitted. *)
      let ms =
        match x with
        | [] -> None
        | [ Ttyp_constr (Path.Pident ms, _, _) ] -> Some ms
        | _ -> raise Not_allowed_function
      in
      {
        arg_type = arg;
        storage_id = s1;
        mut_storage_id = ms;
        return_type = ret;
        return_storage_id = s2;
      }
  | _ -> raise Not_allowed_function

(*
  checking whether the function type is the form of
    <params> -> storage -> <returns> * storage
  or
    <params> -> storage -> mutable storage -> <returns> * storage
*)
let check_valid_signature storage_name mut_storage_name
    { storage_id = s1; mut_storage_id = ms; return_storage_id = s2; _ } =
  (match (ms, mut_storage_name) with
  | Some ms', Some mut_storage_name' -> Ident.same ms' mut_storage_name'
  | None, _ -> true
  | _, _ -> assert false)
  && Ident.same storage_name s1 && Ident.same storage_name s2

(* function signature to ABI *)
let abi_of_signature func_name { arg_type = arg; return_type = ret; _ } =
  let inputs = abi_input_of_typ arg in
  let outputs = abi_output_of_typ ret in
  {
    abi_name = func_name;
    abi_type = Func;
    abi_inputs = inputs;
    abi_outputs = outputs;
    abi_mutability = Nonpayable;
  }

let return_func_of_dispacther { abi_outputs = outputs; _ } =
  let n = List.length outputs in
  gen_return_uint_name n

(* getting decoded parameters passed to the function *)
let params_of_external_function { abi_inputs = inputs; _ } =
  let decoder = function
    | Uint256 -> decode_as_uint
    | Int256 -> decode_as_uint
    | Address -> decode_as_address
  in
  let rec aux n = function
    | [] -> []
    | { input_type = x; _ } :: xs ->
        FunctionCall (decoder x, [ Literal (Dec n) ]) :: aux (n + 1) xs
  in
  aux 0 inputs

let gen_dispacther ~func_name ~storage_num ~mut_storage_num ~is_mut_arg ~abi =
  (*
    If a function name is different from the name of abi, then return None.
    This process is needed because `abi.abi_name` is always different from 
    the function name that the translation of function definitions generate using 
    `Ident.unique_name`.
    In other words,
    in `sig ... val f : <some type> ... end = struct ... let f = <def> ... end`,
    `Ident.unique_name f` at sig...end and `Ident.unique_name f` at struct...end are
    always different.

    If we use `Ident.name` instead of `Ident.unique_name`, conflicting of names can happen.
    For example, if we write `let add (x, y) s = (x + y, s)` in a .ml file,
    then the resulting Yul code includes `function add (x, y, s) -> z, s {...}`
    while `add` is one of the keywords of Yul. Thus, we need to use `Ident.unique_name`.
  *)
  if Ident.name func_name <> abi.abi_name then None
  else
    let func_name = Ident.unique_name func_name in
    let return_num = List.length abi.abi_outputs in
    let params = params_of_external_function abi in
    let return_func = return_func_of_dispacther abi in
    (* preparing fresh variables *)
    let before_storage, returned_vals, after_storage, mut_storage_vals =
      ( List.init storage_num (fun _ -> Utils.fresh_var ()),
        List.init return_num (fun _ -> Utils.fresh_var ()),
        List.init storage_num (fun _ -> Utils.fresh_var ()),
        if is_mut_arg then
          List.init mut_storage_num (fun x ->
              Literal (Dec (x + storage_num + 1)))
        else [] )
    in
    (* If storage is ``empty'' (unit type), then
       both getting and setting storage are omitted. *)
    let call_get_storage, call_set_storage =
      match (before_storage, after_storage) with
      | [], [] -> (None, None)
      | _ :: _, _ :: _ ->
          ( Some
              (Let
                 ( (List.hd before_storage, List.tl before_storage),
                   FunctionCall (get_storage, []) )),
            Some
              (Exp
                 (FunctionCall
                    (set_storage, List.map (fun x -> ID x) after_storage))) )
      | _, _ -> assert false
    in
    let result_vals = returned_vals @ after_storage in
    (* If the function returns some values, then the function call should be let-sentence. *)
    let call_body_function =
      match result_vals with
      | [] ->
          Some
            (Exp
               (FunctionCall
                  ( func_name,
                    params
                    @ List.map (fun x -> ID x) before_storage
                    @ mut_storage_vals )))
      | _ ->
          Some
            (Let
               ( (List.hd result_vals, List.tl result_vals),
                 FunctionCall
                   ( func_name,
                     params
                     @ List.map (fun x -> ID x) before_storage
                     @ mut_storage_vals ) ))
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
                  (return_func, List.map (fun x -> ID x) returned_vals)));
        ]
    in
    Some (Case (keccak_256_for_abi (signature_of_function abi), case_result))

(* generating ABI, default functions, and dispatcher functions *)
let gen_triple_abi_dispatcher_defs { sig_items = sigs; _ } =
  let types, vals = split_module_sig sigs in
  let storage_name, mut_storage_name =
    match types with
    | [ { typ_id = s; _ } ] -> (s, None)
    | [ { typ_id = s1; _ }; { typ_id = s2; _ } ] -> (s1, Some s2)
    | _ -> raise Not_implemented
  in
  let aux = function
    | { val_id = name; val_desc = { ctyp_desc = core_desc; _ }; _ } ->
        let func_sig = uncurry_sig core_desc in
        if check_valid_signature storage_name mut_storage_name func_sig then
          let abi = abi_of_signature (Ident.name name) func_sig in
          ( abi,
            return_uint_def (List.length abi.abi_outputs),
            gen_dispacther
              ~is_mut_arg:
                (match func_sig.mut_storage_id with
                | Some _ -> true
                | None -> false)
              ~abi )
        else raise Not_implemented
  in
  List.fold_left
    (fun (abis, dispatchers) val_desc ->
      let abi, func_def, dispatcher = aux val_desc in
      update_default_function_defs func_def;
      (abi :: abis, dispatcher :: dispatchers))
    ([], []) vals

(* translate the body of the function *)
let translate_function_to_yul func_name e =
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
  let statements, return_vars =
    Anormal_ir.normalize body |> Anormal.normalize rename
    |> Yul_compile.translate_function_body
  in
  FunctionDef (func_name, args, return_vars, statements)

(* translate external functions using `translate_function_to_yul` *)
let translate_external_func = function
  | { vb_pat = { pat_desc = Tpat_var (func_name, _); _ }; vb_expr = e; _ } :: []
    ->
      translate_function_to_yul (Ident.unique_name func_name) e
  | _ -> assert false

(* getting `Case` sentences of the dispather *)
let gen_dispacth_cases cases funcs =
  let rec aux acc = function
    | [] -> acc
    | ({ vb_pat = { pat_desc = Tpat_var (func_name, _); _ }; _ } :: []) :: rest
      ->
        aux (acc @ List.filter_map (fun x -> x ~func_name) cases) rest
    | _ -> assert false
  in
  aux [] funcs

(* splitting module structure to types and external vals and internal vals *)
let rec split_module_str = function
  | [] -> ([], [])
  | x :: xs -> (
      let types, exps = split_module_str xs in
      match x.str_desc with
      | Tstr_type (_, y) -> (y @ types, exps)
      | Tstr_value (_, y) -> (types, y :: exps)
      | Tstr_open _ -> (types, exps)
      | _ -> raise Not_implemented)

(* translating Yul to the .json file which includes ABI, bytecode, deployedBytecode, etc,
   using solc not solc-js *)
let json_of_yul abis yul_code contract_name =
  let yul_code = json_string_of_yul yul_code in
  let yul_json : Yojson.Basic.t =
    `Assoc
      [
        ("language", `String "Yul");
        ( "sources",
          `Assoc [ ("destructible", `Assoc [ ("content", `String yul_code) ]) ]
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
  List.fold_left Yojson.Basic.Util.combine
    (`Assoc [ ("contractName", `String contract_name) ])
    [ json_of_abis abis; bytecode; deployed_bytecode ]
  |> Yojson.Basic.pretty_to_string

let write_json_contract source_file contract_name result_json =
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

let backend source_file Typedtree.{ structure; _ } =
  match structure with
  | { str_items = items; _ } ->
      (* ignoring "open M" *)
      let rec backend_aux = function
        | { str_desc = Tstr_open _; _ } :: tl -> backend_aux tl
        | [
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
                                mod_desc =
                                  Tmod_structure { str_items = items; _ };
                                _;
                              },
                              _,
                              Tmodtype_explicit
                                { mty_desc = Tmty_signature sigs; _ },
                              _ );
                        _;
                      };
                    _;
                  };
              _;
            };
          ] ->
            let contract_name = Ident.name contract_name in
            let abis, cases = gen_triple_abi_dispatcher_defs sigs in
            (* getting val expression (types are not yet implemented) *)
            let types, exps = split_module_str items in
            (* getting the number of storage *)
            let storage_num, mut_storage_num = get_storage_num types in
            (* getting function declarations and dispatcher cases *)
            let funcs = List.map translate_external_func exps in
            let cases =
              List.map
                (fun (case :
                       func_name:Ident.t ->
                       storage_num:int ->
                       mut_storage_num:int ->
                       Yul_ast.case option) ->
                  case ~storage_num ~mut_storage_num)
                cases
            in
            let cases = gen_dispacth_cases cases exps in
            (* generating and adding set/get storage functions *)
            if storage_num > 0 then (
              update_default_function_defs (get_storage_def storage_num);
              update_default_function_defs (set_storage_def storage_num))
            else ();
            if mut_storage_num > 0 then
              update_default_function_defs get_hash_slot_def
            else ();
            (* code fragment: dispatcher *)
            let dispatcher =
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
                         Code
                           ((dispatcher :: funcs) @ get_default_function_defs ()),
                         None )) )
            in
            let result_json = json_of_yul abis yul_code contract_name in
            write_json_contract source_file contract_name result_json
        | _ -> raise Not_implemented
      in
      (* before compilation, need to reset default functions to avoid name confliction *)
      reset_default_function_defs ();
      backend_aux items
