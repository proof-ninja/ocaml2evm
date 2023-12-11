open Asttypes
open Typedtree

type typedid = string * Types.type_expr
type bop = Add | Sub | Mul | Div

type aval =
  | Var of string
  | IntV of int
  | BoolV of bool
  | StrV of string
  | UnitV

type funexp =
  | Val of aval
  | Bop of (aval * bop * aval)
  | App of (aval * aval list)

type acexp =
  | Fexp of funexp
  (* | Val of aval
     | Bop of (aval * bop * aval)
     | App of (aval * aval list) *)
  | Tuple of aval list

type aexp = ACexp of acexp | ALetin of typedid * acexp * aexp
type pexp = Cexp of acexp | Letin of string list * funexp * pexp

let fresh_var =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1;
    "___" ^ "var" ^ "_" ^ string_of_int v ^ "___"
  in
  body

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

let rec normalize_aux { exp_desc = e; exp_type = _; _ } k =
  match e with
  | Texp_ident (Pident s, _, _) -> Fexp (Val (Var (Ident.unique_name s))) |> k
  | Texp_constant x -> Fexp (Val (const_to_aval x)) |> k
  | Texp_construct ({ txt = Longident.Lident s; _ }, _, _) ->
      Fexp
        (Val
           (if s = "true" then BoolV true
            else if s = "false" then BoolV false
            else if s = "()" then UnitV
            else assert false))
      |> k
  | Texp_apply (f, args) -> (
      let args = List.filter_map (fun x -> match x with _, y -> y) args in
      match (f, args) with
      | { exp_desc = Texp_ident (Pdot (_, s), _, _); _ }, [ e1; e2 ] ->
          let b = get_bop s in
          normalize_name e1 (fun e1' ->
              normalize_name e2 (fun e2' -> k (Fexp (Bop (e1', b, e2')))))
      | e1, args ->
          normalize_name e1 (fun e1' ->
              let rec args_aux acc = function
                | [] -> k (Fexp (App (e1', List.rev acc)))
                | x :: xs ->
                    normalize_name x (fun x' -> args_aux (x' :: acc) xs)
              in
              args_aux [] args))
  | Texp_let
      ( _,
        [ { vb_pat = { pat_desc = p; pat_type = t; _ }; vb_expr = e1; _ } ],
        e2 ) ->
      let var =
        match p with
        | Tpat_var (s, _) -> Ident.unique_name s
        | _ -> assert false
      in
      normalize_aux e1 (fun e1' -> ALetin ((var, t), e1', normalize_aux e2 k))
  | Texp_tuple el ->
      let rec tuple_aux acc = function
        | [] -> k (Tuple (List.rev acc))
        | x :: xs -> normalize_name x (fun x' -> tuple_aux (x' :: acc) xs)
      in
      tuple_aux [] el
  | _ -> assert false

and normalize_name e k =
  normalize_aux e (fun e' ->
      match e' with
      | Fexp (Val v) -> k v
      | _ ->
          let var = fresh_var () in
          ALetin ((var, e.exp_type), e', Var var |> k))

let count_vars t =
  let open Types in
  let t = get_desc t in
  match t with
  (* | Tarrow (_, t1, t2, _) -> count_vars t1 @ count_vars t2 *)
  | Ttuple tl ->
      let rec count_vars_aux t' =
        let t' = get_desc t' in
        match t' with
        (* | Tarrow (_, t1, t2, _) -> count_vars_aux t1 @ count_vars_aux t2 *)
        | Ttuple tl' -> List.fold_left (fun x y -> x @ count_vars_aux y) [] tl'
        | Tconstr (_, [], _) -> [ fresh_var () ]
        | Tvar _ -> [ fresh_var () ]
        | _ -> assert false
      in
      Some (List.fold_left (fun x y -> x @ count_vars_aux y) [] tl)
  | _ -> None

let rename_avals l rename =
  List.fold_left
    (fun x y ->
      match y with
      | Var s -> (
          match List.find_opt (fun (z, _) -> s = z) rename with
          | Some (_, ids) -> x @ List.map (fun x -> Var x) ids
          | None -> x @ [ Var s ])
      | _ -> y :: x)
    [] l

let rename_cexp e rename =
  match e with
  | Fexp e' -> (
      match e' with
      | Val (Var id) -> (
          match List.find_opt (fun (x, _) -> x = id) rename with
          | Some (_, ids) -> Tuple (List.map (fun x -> Var x) ids)
          | None -> Fexp (Val (Var id)))
      | App (f, args) -> Fexp (App (f, rename_avals args rename))
      | _ -> Fexp e')
  | Tuple el -> Tuple (rename_avals el rename)

let rec remove_tuple e rename =
  match e with
  | ACexp e' -> Cexp (rename_cexp e' rename)
  | ALetin ((x, t), e1, e2) -> (
      match count_vars t with
      | Some xs -> (
          let rename = (x, xs) :: rename in
          let e1' = rename_cexp e1 rename in
          let e2' = remove_tuple e2 rename in
          match e1' with
          | Tuple el ->
              let rec gen_tuple_let = function
                | [ x ], [ y ] -> Letin ([ x ], Val y, e2')
                | x :: xs, y :: ys ->
                    Letin ([ x ], Val y, gen_tuple_let (xs, ys))
                | _ -> assert false
              in
              gen_tuple_let (xs, el)
          | Fexp e1'' -> Letin (xs, e1'', e2'))
      | None -> (
          let e1' = rename_cexp e1 rename in
          let e2' = remove_tuple e2 rename in
          match e1' with
          | Tuple _ -> assert false
          | Fexp e1'' -> Letin ([ x ], e1'', e2')))

let normalize_with_tuple e = normalize_aux e (fun x -> ACexp x)
let normalize e = remove_tuple (normalize_with_tuple e)
let string_of_bop = function Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"

let string_of_aval = function
  | Var s -> s
  | IntV n -> string_of_int n
  | BoolV b -> string_of_bool b
  | StrV s -> s
  | UnitV -> "()"

let string_of_fexp = function
  | Val v -> string_of_aval v
  | Bop (v1, op, v2) ->
      string_of_aval v1 ^ " " ^ string_of_bop op ^ " " ^ string_of_aval v2
  | App (f, xs) ->
      string_of_aval f
      ^ List.fold_left (fun acc x -> acc ^ " " ^ string_of_aval x) "" xs

let string_of_acexp = function
  | Fexp e -> string_of_fexp e
  | Tuple vs ->
      "("
      ^ List.fold_left (fun acc x -> acc ^ ", " ^ string_of_aval x) "" vs
      ^ ")"

(* let rec string_of_aexp = function
   | ACexp e -> string_of_acexp e
   | ALetin ((x, _), e1, e2) ->
       "let " ^ x ^ " = " ^ string_of_acexp e1 ^ " in " ^ string_of_aexp e2 *)

let rec string_of_pexp e =
  match e with
  | Cexp e -> string_of_acexp e
  | Letin (vars, e1, e2) ->
      "let"
      ^ List.fold_left (fun acc x -> acc ^ ", " ^ x) "" vars
      ^ " = " ^ string_of_fexp e1 ^ " in " ^ string_of_pexp e2
