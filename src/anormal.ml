open Asttypes
open Typedtree

type bop = Add | Sub | Mul | Div

type aval =
  | Var of string
  | IntV of int
  | BoolV of bool
  | StrV of string
  | UnitV

type acexp =
  | AVal of aval
  | ABop of (aval * bop * aval)
  | AApp of (aval * aval list * Types.type_expr)
  | ATuple of aval list

type letexp =
  | LVal of aval
  | LBop of (aval * bop * aval)
  | LApp of (aval * aval list)

type resexp = RVal of aval | RTuple of aval list

type aexp =
  | ACexp of acexp
  | ALetin of (string list * (string * string list) list) * acexp * aexp

type pexp = Rexp of resexp | Letin of string list * letexp * pexp

let string_of_bop = function Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"

let string_of_aval = function
  | Var s -> s
  | IntV n -> string_of_int n
  | BoolV b -> string_of_bool b
  | StrV s -> s
  | UnitV -> "()"

let string_of_acexp = function
  | AVal v -> string_of_aval v
  | ABop (v1, op, v2) ->
      string_of_aval v1 ^ " " ^ string_of_bop op ^ " " ^ string_of_aval v2
  | AApp (f, args, _) ->
      string_of_aval f
      ^ List.fold_left (fun acc x -> acc ^ " " ^ string_of_aval x) "" args
  | ATuple vs ->
      "("
      ^ List.fold_left (fun acc x -> acc ^ ", " ^ string_of_aval x) "" vs
      ^ ")"

let string_of_letexp = function
  | LVal v -> string_of_aval v
  | LBop (v1, op, v2) ->
      string_of_aval v1 ^ " " ^ string_of_bop op ^ " " ^ string_of_aval v2
  | LApp (f, xs) ->
      string_of_aval f
      ^ List.fold_left (fun acc x -> acc ^ " " ^ string_of_aval x) "" xs

let string_of_rexp = function
  | RVal v -> string_of_aval v
  | RTuple vs ->
      "("
      ^ List.fold_left (fun acc x -> acc ^ ", " ^ string_of_aval x) "" vs
      ^ ")"

let rec string_of_aexp = function
  | ACexp e -> string_of_acexp e
  | ALetin ((xs, _), e1, e2) ->
      "let "
      ^ List.fold_left (fun acc x -> acc ^ ", " ^ x) "" xs
      ^ " = " ^ string_of_acexp e1 ^ " in " ^ string_of_aexp e2

let rec string_of_pexp e =
  match e with
  | Rexp e -> string_of_rexp e
  | Letin (vars, e1, e2) ->
      "let"
      ^ List.fold_left (fun acc x -> acc ^ ", " ^ x) "" vars
      ^ " = " ^ string_of_letexp e1 ^ " in " ^ string_of_pexp e2

let const_to_aval = function
  | Const_int n -> IntV n
  | Const_string (s, _, _) -> StrV s
  | _ -> assert false

let get_bop s =
  if s = "+" then Add
  else if s = "-" then Sub
  else if s = "*" then Mul
  else if s = "/" then Div
  else assert false

let count_vars t =
  let open Types in
  let t = get_desc t in
  match t with
  | Ttuple tl ->
      let rec count_vars_aux t' =
        let t' = get_desc t' in
        match t' with
        | Ttuple tl' -> List.fold_left (fun x y -> x @ count_vars_aux y) [] tl'
        | Tconstr (Path.Pident p, [], _) ->
            if Ident.name p = "unit" then [] else [ Utils.fresh_var () ]
        | Tvar _ -> [ Utils.fresh_var () ]
        | _ -> assert false
      in
      Some (List.fold_left (fun x y -> x @ count_vars_aux y) [] tl)
  | _ -> None

let rec normalize_aux { exp_desc = e; exp_type = t; _ } k =
  match e with
  | Texp_ident (Pident s, _, _) -> (AVal (Var (Ident.unique_name s)), t) |> k
  | Texp_constant x -> (AVal (const_to_aval x), t) |> k
  | Texp_construct ({ txt = Longident.Lident s; _ }, _, _) ->
      ( AVal
          (if s = "true" then BoolV true
           else if s = "false" then BoolV false
           else if s = "()" then UnitV
           else assert false),
        t )
      |> k
  | Texp_apply (f, args) -> (
      let args = List.filter_map (fun x -> match x with _, y -> y) args in
      match (f, args) with
      | { exp_desc = Texp_ident (Pdot (_, s), _, _); _ }, [ e1; e2 ] ->
          let b = get_bop s in
          normalize_name e1 (fun e1' ->
              normalize_name e2 (fun e2' -> (ABop (e1', b, e2'), t) |> k))
      | e1, args ->
          normalize_name e1 (fun e1' ->
              let rec args_aux acc = function
                | [] -> (AApp (e1', List.rev acc, t), t) |> k
                | x :: xs ->
                    normalize_name x (fun x' -> args_aux (x' :: acc) xs)
              in
              args_aux [] args))
  | Texp_let (_, [ { vb_pat = p; vb_expr = e1; _ } ], e2) ->
      (* let var =
           match p with
           | Tpat_var (s, _) -> Ident.unique_name s
           | _ -> assert false
         in *)
      let rename, vars = Utils.flatten_tuple_pat p in
      normalize_aux e1 (fun (e1', _) ->
          ALetin ((vars, rename), e1', normalize_aux e2 k))
  | Texp_tuple el ->
      let rec tuple_aux acc = function
        | [] -> (ATuple (List.rev acc), t) |> k
        | x :: xs -> normalize_name x (fun x' -> tuple_aux (x' :: acc) xs)
      in
      tuple_aux [] el
  | Texp_match
      (e1, [ { c_lhs = { pat_desc = Tpat_value p; _ }; c_rhs = e2; _ } ], _) ->
      let rename, vars = Utils.flatten_tuple_pat (p :> pattern) in
      normalize_aux e1 (fun (e1', _) ->
          ALetin ((vars, rename), e1', normalize_aux e2 k))
  | _ -> assert false

and normalize_name e k =
  normalize_aux e (fun (e', t) ->
      match e' with
      | AVal v -> k v
      | _ -> (
          let var = Utils.fresh_var () in
          match count_vars t with
          | Some xs -> ALetin (([ var ], [ (var, xs) ]), e', Var var |> k)
          | None -> ALetin (([ var ], []), e', Var var |> k)))

let rename_ids l rename =
  List.fold_left
    (fun acc y ->
      match List.find_opt (fun (z, _) -> y = z) rename with
      | Some (_, ids) -> acc @ ids
      | None -> acc @ [ y ])
    [] l

let rename_avals l rename =
  List.fold_left
    (fun acc y ->
      match y with
      | Var s -> (
          match List.find_opt (fun (z, _) -> s = z) rename with
          | Some (_, ids) -> acc @ List.map (fun x -> Var x) ids
          | None -> acc @ [ Var s ])
      | _ -> acc @ [ y ])
    [] l

let rename_cexp e rename =
  match e with
  | AVal (Var id) -> (
      match List.find_opt (fun (x, _) -> x = id) rename with
      | Some (_, ids) -> ATuple (List.map (fun x -> Var x) ids)
      | None -> AVal (Var id))
  | AApp (f, args, t) -> AApp (f, rename_avals args rename, t)
  | ATuple el -> ATuple (rename_avals el rename)
  | _ -> e

let rename_cexp_to_pexp e rename =
  match e with
  | AVal (Var id) -> (
      match List.find_opt (fun (x, _) -> x = id) rename with
      | Some (_, ids) -> Rexp (RTuple (List.map (fun x -> Var x) ids))
      | None -> Rexp (RVal (Var id)))
  | AVal v -> Rexp (RVal v)
  | ABop args ->
      let res_var = Utils.fresh_var () in
      Letin ([ res_var ], LBop args, Rexp (RVal (Var res_var)))
  | AApp (f, args, t) -> (
      match count_vars t with
      | Some vars ->
          Letin
            ( vars,
              LApp (f, rename_avals args rename),
              Rexp (RTuple (List.map (fun x -> Var x) vars)) )
      | None ->
          let res_var = Utils.fresh_var () in
          Letin
            ( [ res_var ],
              LApp (f, rename_avals args rename),
              Rexp (RVal (Var res_var)) ))
  | ATuple el -> Rexp (RTuple (rename_avals el rename))

let rec remove_tuple e rename =
  match e with
  | ACexp e' -> rename_cexp_to_pexp e' rename
  | ALetin ((vars, new_rename), e1, e2) -> (
      let rename = new_rename @ rename in
      let vars = rename_ids vars rename in
      let e1' = rename_cexp e1 rename in
      let e2' = remove_tuple e2 rename in
      match e1' with
      | ATuple el ->
          let rec gen_tuple_let = function
            | [ x ], [ y ] -> Letin ([ x ], LVal y, e2')
            | x :: xs, y :: ys -> Letin ([ x ], LVal y, gen_tuple_let (xs, ys))
            | _ -> assert false
          in
          gen_tuple_let (vars, el)
      | AVal arg -> Letin (vars, LVal arg, e2')
      | ABop args -> Letin (vars, LBop args, e2')
      | AApp (f, args, _) -> Letin (vars, LApp (f, args), e2'))

let normalize_with_tuple e = normalize_aux e (fun (x, _) -> ACexp x)
let normalize e = remove_tuple (normalize_with_tuple e)
