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
  | _ -> (e, mut)

let cexp_to_exp e =
  match e with
  | AVal v -> Rexp (RVal v)
  | AApp (f, args, t) -> (
      match Utils.count_vars_in_type t with
      | Some vars ->
          Letin
            ( vars,
              LApp (f, args),
              Rexp (RTuple (List.map (fun x -> Var x) vars)) )
      | None ->
          let res_var = Utils.fresh_var () in
          Letin ([ res_var ], LApp (f, args), Rexp (RVal (Var res_var))))
  | ATuple el -> Rexp (RTuple el)

let rec remove_tuple rename e mut =
  match e with
  | ACexp e' ->
      let e, mut = rename_cexp rename e' mut in
      (cexp_to_exp e, mut)
  | ASeq (e1, e2) -> (
      match rename_cexp rename e1 mut with
      | AApp (f, args, _), mut ->
          let e, mut = remove_tuple rename e2 mut in
          (Seq (LApp (f, args), e), mut)
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
      | AApp (f, args, _) -> (Letin (vars, LApp (f, args), e2'), mut))

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
