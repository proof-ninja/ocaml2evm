open Yul_ast
open Normalized_common_ast
open Normalized_ast

let aval_to_yul = function
  | Var s -> ID s
  | IntV n -> Literal (hex_of_int n)
  | BoolV b -> if b then Literal (Dec 1) else Literal (Dec 0)
  | StrV s -> Literal (Str (Strlit s))
  | _ -> assert false

let translate_aval_args v =
  match v with UnitV -> None | _ -> Some (aval_to_yul v)

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
  | LApp (Bop b, [ v1; v2 ]) -> (
      let v1 = aval_to_yul v1 in
      let v2 = aval_to_yul v2 in
      let gen_bop_call f name =
        update_default_function_defs f;
        FunctionCall (name, [ v1; v2 ])
      in
      match b with
      | UAdd -> gen_bop_call uint_add_def uint_add
      | USub -> gen_bop_call uint_sub_def uint_sub
      | UMul -> gen_bop_call uint_mul_def uint_mul
      | UDiv -> gen_bop_call uint_div_def uint_div
      | SAdd -> gen_bop_call sint_add_def sint_add
      | SSub -> gen_bop_call sint_sub_def sint_sub
      | SMul -> gen_bop_call sint_mul_def sint_mul
      | SDiv -> gen_bop_call sint_div_def sint_div)
  | LApp (HashReplace, [ h; x; y ]) ->
      EVM
        (Sstore
           ( FunctionCall (get_hash_slot, [ aval_to_yul h; aval_to_yul x ]),
             aval_to_yul y ))
  | LApp (HashFind, [ h; x ]) ->
      EVM
        (Sload (FunctionCall (get_hash_slot, [ aval_to_yul h; aval_to_yul x ])))
  | LApp (Caller, [ UnitV ]) -> EVM Caller
  | LIf _ -> assert false
  | _ -> assert false

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
  | If _ -> assert false

let translate_function_body e =
  let statements, return_vars = translate_body_aux e [] in
  (List.rev statements, List.rev return_vars)

let translate_function
    { name = func_name; arg_ids = args; body; mutability = mut } =
  let statements, return_vars = translate_function_body body in
  ( FunctionDef (Ident.unique_name func_name, args, return_vars, statements),
    (Ident.name func_name, mut) )
