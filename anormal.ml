open Asttypes
open Typedtree

type bop = Add | Sub | Mul | Div

type aval =
  | Var of string
  | IntV of int
  | BoolV of bool
  | StrV of string
  | UnitV

and acexp =
  | Val of aval
  | Bop of (aval * bop * aval)
  | App of (aval * aval)
  | Tuple of aval list

and aexp = Cexp of acexp | Letin of string * acexp * aexp

let fresh_var =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1;
    "___" ^ "var" ^ "_" ^ string_of_int v ^ "___"
  in
  body

let is_aval = function Val _ -> true | _ -> false

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

let rec normalize_aux e k =
  match e with
  | Texp_ident (Pident s, _, _) -> Val (Var (Ident.unique_name s)) |> k
  | Texp_constant x -> Val (const_to_aval x) |> k
  | Texp_construct ({ txt = Longident.Lident s; _ }, _, _) ->
      if s = "true" then Val (BoolV true) |> k
      else if s = "false" then Val (BoolV false) |> k
      else if s = "()" then Val UnitV |> k
      else assert false
  | Texp_apply (f, args) -> (
      let f, args =
        match (f, args) with
        | { exp_desc = e1; _ }, args ->
            ( e1,
              List.filter_map (fun x -> match x with _, y -> y) args
              |> List.map (fun x -> match x with { exp_desc = e; _ } -> e) )
      in
      match (f, args) with
      | Texp_ident (Pdot (_, s), _, _), [ e1; e2 ] ->
          let b = get_bop s in
          normalize_name e1 (fun e1' ->
              normalize_name e2 (fun e2' -> k (Bop (e1', b, e2'))))
      | e1, [ e2 ] ->
          normalize_name e1 (fun e1' ->
              normalize_name e2 (fun e2' -> k (App (e1', e2'))))
      | _ -> assert false)
  | Texp_let
      ( _,
        [ { vb_pat = { pat_desc = p; _ }; vb_expr = { exp_desc = e1; _ }; _ } ],
        { exp_desc = e2; _ } ) ->
      let var =
        match p with
        | Tpat_var (s, _) -> Ident.unique_name s
        | _ -> assert false
      in
      normalize_aux e1 (fun e1' -> Letin (var, e1', normalize_aux e2 k))
  | Texp_tuple [ { exp_desc = e1; _ }; { exp_desc = e2; _ } ] ->
      normalize_name e1 (fun e1' ->
          normalize_name e2 (fun e2' -> k (Tuple [ e1'; e2' ])))
  | _ -> assert false

and normalize_name e k =
  normalize_aux e (fun e' ->
      match e' with
      | Val v -> k v
      | _ ->
          let var = fresh_var () in
          Letin (var, e', Var var |> k))

let normalize e = normalize_aux e (fun x -> Cexp x)
