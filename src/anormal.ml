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

let rename_cexp rename e =
  match e with
  | AVal (Var id) -> (
      match List.find_opt (fun (x, _) -> x = id) rename with
      | Some (_, ids) -> ATuple (List.map (fun x -> Var x) ids)
      | None -> AVal (Var id))
  | AApp (f, args, t) -> AApp (f, rename_avals rename args, t)
  | ATuple el -> ATuple (rename_avals rename el)
  | _ -> e

let rename_cexp_to_exp rename e =
  match e with
  | AVal (Var id) -> (
      match List.find_opt (fun (x, _) -> x = id) rename with
      | Some (_, ids) -> Rexp (RTuple (List.map (fun x -> Var x) ids))
      | None -> Rexp (RVal (Var id)))
  | AVal v -> Rexp (RVal v)
  | AApp (f, args, t) -> (
      match Utils.count_vars_in_type t with
      | Some vars ->
          Letin
            ( vars,
              LApp (f, rename_avals rename args),
              Rexp (RTuple (List.map (fun x -> Var x) vars)) )
      | None ->
          let res_var = Utils.fresh_var () in
          Letin
            ( [ res_var ],
              LApp (f, rename_avals rename args),
              Rexp (RVal (Var res_var)) ))
  | ATuple el -> Rexp (RTuple (rename_avals rename el))

let rec remove_tuple rename e =
  match e with
  | ACexp e' -> rename_cexp_to_exp rename e'
  | ASeq (e1, e2) -> (
      match rename_cexp rename e1 with
      | AApp (f, args, _) -> Seq (LApp (f, args), remove_tuple rename e2)
      | _ -> assert false)
  | ALetin ((vars, new_rename), e1, e2) -> (
      let rename = new_rename @ rename in
      let vars = rename_ids rename vars in
      let e1' = rename_cexp rename e1 in
      let e2' = remove_tuple rename e2 in
      match e1' with
      | ATuple el ->
          let rec gen_tuple_let = function
            | [ x ], [ y ] -> Letin ([ x ], LVal y, e2')
            | x :: xs, y :: ys -> Letin ([ x ], LVal y, gen_tuple_let (xs, ys))
            | _ -> assert false
          in
          gen_tuple_let (vars, el)
      | AVal arg -> Letin (vars, LVal arg, e2')
      | AApp (f, args, _) -> Letin (vars, LApp (f, args), e2'))

let normalize = remove_tuple
