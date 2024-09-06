open Normalized_common_ast
open Normalized_ir_ast
open Normalized_ast

let rename_ids rename l =
  List.fold_left
    (fun acc y ->
      match List.find_opt (fun (z, _) -> y = z) rename with
      | Some (_, ids) -> acc @ ids
      | None -> acc @ [ y ])
    [] l

let rename_avals rename l =
  List.fold_left
    (fun acc y ->
      match y with
      | Var s -> (
          match List.find_opt (fun (z, _) -> s = z) rename with
          | Some (_, ids) -> acc @ List.map (fun x -> Var x) ids
          | None -> acc @ [ Var s ])
      | _ -> acc @ [ y ])
    [] l

let application_mut mut = function
  | Bop _ | HashFind | Caller -> Abi.stronger_mutability View mut
  | _ -> Abi.stronger_mutability Nonpayable mut

let cexp_to_exp e =
  let open Types in
  match e with
  | AVal v -> Rexp (RVal v)
  | AApp (f, args, t) -> (
      let x = (match get_desc t with Tconstr (Path.Pident p, [], _) -> if Ident.name p = "unit" then Some (Seq(LApp (f, args),Rexp (RVal UnitV))) else None| _ -> None) in
      match x with 
      Some x' -> x'
      | _ ->(
        match Utils.count_vars_in_type t with
        | Some vars ->
            Letin
              ( vars,
                LApp (f, args),
                Rexp (RTuple (List.map (fun x -> Var x) vars)) )
        | None ->
            let res_var = Utils.fresh_var () in
            Letin ([ res_var ], LApp (f, args), Rexp (RVal (Var res_var)))))
  | ATuple el -> Rexp (RTuple el)
  | AIf _ -> assert false

let rename_cexp rename e mut =
  match e with
  | AVal (Var id) -> (
      match List.find_opt (fun (x, _) -> x = id) rename with
      | Some (_, ids) -> (ATuple (List.map (fun x -> Var x) ids), mut)
      | None -> (AVal (Var id), mut))
  | AApp (f, args, t) ->
      let mut = application_mut mut f in
      (AApp (f, rename_avals rename args, t), mut)
  | ATuple el -> (ATuple (rename_avals rename el), mut)
  | AIf (v, e1, e2) -> 
    let v2 = match v with
      | Var id -> (
        let v2' = match List.find_opt (fun (x, _) -> x = id) rename with
          | Some (_, ids) -> (match ids with [v] -> Var v | _ -> assert false)
          | None -> Var id in v2')
      | _ -> v in
    (AIf (v2, e1, e2), mut)
  | _ -> (e, mut)

let rec remove_tuple rename e mut : exp * Abi.state_mutability =
  match e with
  | ACexp e' ->
      let e, mut = rename_cexp rename e' mut in
      (match e with 
      | AIf (v, e1, e2) -> (
        let e1', mut1 = remove_tuple rename e1 mut in
        let e2', mut2 = remove_tuple rename e2 mut in
        (If(v, e1', e2'), Abi.stronger_mutability mut1 mut2))
      | _ -> (cexp_to_exp e, mut))
  | ASeq (e1, e2) -> (
      match rename_cexp rename e1 mut with
      | AApp (f, args, _), mut ->
          let e, mut = remove_tuple rename e2 mut in
          (Seq (LApp (f, args), e), mut)
      | AIf (v, e11, e12), mut -> 
        let e11', mut1 = remove_tuple rename e11 mut in
        let e12', mut2 = remove_tuple rename e12 mut in 
        let e, mut = remove_tuple rename e2 (Abi.stronger_mutability mut1 mut2) in
        (Seq (LIf (v, e11', e12'), e), mut)
      | _ -> assert false)
  | ALetin ((vars, new_rename), e1, e2) -> (
      let rename = new_rename @ rename in
      let vars = rename_ids rename vars in
      let e1', mut = rename_cexp rename e1 mut in
      let e2', mut = remove_tuple rename e2 mut in
      match e1' with
      | ATuple el ->
          let rec gen_tuple_let = function
            | [ x ], [ y ] -> Letin ([ x ], LVal y, e2')
            | x :: xs, y :: ys -> Letin ([ x ], LVal y, gen_tuple_let (xs, ys))
            | _ -> assert false
          in
          (gen_tuple_let (vars, el), mut)
      | AVal arg -> (Letin (vars, LVal arg, e2'), mut)
      | AApp (f, args, _) -> (Letin (vars, LApp (f, args), e2'), mut)
      | AIf (v, e1'', e2'') -> 
        let e3, mut1 = remove_tuple rename e1'' mut in
        let e4, mut2 = remove_tuple rename e2'' mut in
        (Letin (vars, LIf(v, e3, e4), e2'), Abi.stronger_mutability mut1 mut2))
      
let normalize { name = func_name; arg_pats = args; body; mutability = mut } =
  let renames, args =
    List.fold_left
      (fun (acc_fst, acc_snd) x ->
        let rename, arg = Utils.flatten_tuple_pat x in
        (acc_fst @ rename, acc_snd @ arg))
      ([], []) args
  in
  let body, mut = remove_tuple renames body mut in
  { name = func_name; arg_ids = args; body; mutability = mut }
