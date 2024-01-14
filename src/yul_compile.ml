open Yul_ast
open Normalized_common_ast
open Normalized_ast

let aval_to_yul = function
  | Var s -> ID s
  | IntV n -> Literal (Dec n)
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
      match b with
      | Add ->
          update_default_function_defs safe_add_def;
          FunctionCall (safe_add, [ v1; v2 ])
      | Sub ->
          update_default_function_defs safe_sub_def;
          FunctionCall (safe_sub, [ v1; v2 ])
      | Mul ->
          update_default_function_defs safe_mul_def;
          FunctionCall (safe_mul, [ v1; v2 ])
      | Div ->
          update_default_function_defs safe_div_def;
          FunctionCall (safe_div, [ v1; v2 ]))
  | LApp (HashReplace, [ h; x; y ]) ->
      EVM
        (Sstore
           ( FunctionCall (get_hash_slot, [ aval_to_yul h; aval_to_yul x ]),
             aval_to_yul y ))
  | LApp (HashFind, [ h; x ]) ->
      EVM
        (Sload (FunctionCall (get_hash_slot, [ aval_to_yul h; aval_to_yul x ])))
  | LApp (Caller, [ UnitV ]) -> EVM Caller
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

let translate_function_body e =
  let statements, return_vars = translate_body_aux e [] in
  (List.rev statements, List.rev return_vars)
