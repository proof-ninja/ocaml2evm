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

(* The case is divided by whether new variables for the return values are needed.
   If ret_vars is empty, new variables are needed, and if not, ret_vars is the list of names of return values. *)
let return_exp vals acc (ret_vars:id list (*option*)) =
  let vals = List.filter (fun x -> not (x = UnitV)) vals in
  let rec assign_rets vals acc_exp acc_rets ret_vars'=
    match vals with
    | [] -> (acc_exp, acc_rets)
    | v :: vs ->
        let ret, ret_vars' = (match ret_vars' with [] -> Utils.fresh_var (), [] | hd :: tl -> hd, tl ) in
        assign_rets vs
          (Assign ((ret, []), aval_to_yul v) :: acc_exp)
          (ret :: acc_rets)
          ret_vars'
  in
  assign_rets vals acc [] ret_vars

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
      | SDiv -> gen_bop_call sint_div_def sint_div
      | And -> assert false
      | Or -> assert false
      | Eq -> EVM (Eq (v1, v2))
      | Neq -> EVM (Iszero (EVM (Eq (v1, v2))))
      | Lt -> EVM (Lt (v1, v2))
      | Gt -> EVM (Gt (v1, v2))
      | Lte -> EVM (Iszero (EVM (Gt (v1, v2))))
      | Gte -> EVM (Iszero (EVM (Lt (v1, v2))))
      | _ -> assert false)
  | LApp (Bop Not,[v]) -> let v = aval_to_yul v in EVM (Iszero v)
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

  (* an argument "ret_vars" denotes the list of names of return variables.
  if ret_vars is empty, it means we have to make new variables.*)
let rec translate_body_aux e acc ret_vars=
  match e with
  | Rexp e' -> (
      match e' with
      | RTuple vals -> return_exp vals acc ret_vars
      | RVal v -> return_exp [ v ] acc ret_vars)
  | Seq (e1, e2) ->
      let acc = (match e1 with LIf (v, e1', e2') -> 
        let e1_block, _ = translate_body_aux e1' [] [] in
        let e1_block = List.rev e1_block in
        let e2_block, _ = translate_body_aux e2' [] ret_vars in
        let e2_block = List.rev e2_block in
        let v = aval_to_yul v in
        Switch (v, [Case(Dec 1, e1_block); Case(Dec 0, e2_block)], Default []) :: acc   (*initialization with 0*)
        | _ -> Exp (letexp_to_yul e1) :: acc) in
      (* let acc = Exp (letexp_to_yul e1) :: acc in *)
      translate_body_aux e2 acc ret_vars
  | Letin (vars, e1, e2) ->
      let a = (match e1 with LIf (v, e1', e2') -> 
        let e1_block, _ = translate_body_aux e1' [] vars in
        let e1_block = List.rev e1_block in
        let e2_block, _ = translate_body_aux e2' [] vars in
        let e2_block = List.rev e2_block in
        let v = aval_to_yul v in  
        let acc = Switch (v, [Case(Dec 1, e1_block); Case(Dec 0, e2_block)], Default []) :: (Let((List.hd vars, List.tl vars), Literal (Dec 0))) :: acc in   (*initialization with 0*)
        translate_body_aux e2 acc ret_vars
        | LApp (Bop ((And | Or) as b), [v1; v2]) -> 
          assert (List.length vars = 1);
          let var = List.hd vars in
          let v1 = aval_to_yul v1 in
          let v2 = aval_to_yul v2 in
          let bexp = (match b with And -> v1 | Or -> EVM (Iszero v1)| _ -> assert false) in
          let acc = (Yul_ast.If (bexp, [Assign ((var, []), v2)])) :: Let ((var, []), v1) :: acc in
          translate_body_aux e2 acc ret_vars
        |_ -> let acc = Let ((List.hd vars, List.tl vars), letexp_to_yul e1) :: acc in
        translate_body_aux e2 acc ret_vars) in a
  | If (v, e1, e2) -> 
      let e1', vars1 = translate_body_aux e1 acc ret_vars in
      let e1' = List.rev e1' in
      let e2', vars2 = translate_body_aux e2 acc vars1 in
      let e2' = List.rev e2' in
      assert (vars1 = vars2);
      let v = aval_to_yul v in
      Switch (v, [Case(Dec 1, e1'); Case(Dec 0, e2')], Default []) :: acc, vars1

let translate_function_body e =
  let statements, return_vars = translate_body_aux e [] [] in
  (List.rev statements, List.rev return_vars)

let translate_function
    { name = func_name; arg_ids = args; body; mutability = mut } =
  let statements, return_vars = translate_function_body body in
  ( FunctionDef (Ident.unique_name func_name, args, return_vars, statements),
    (Ident.name func_name, mut) )
