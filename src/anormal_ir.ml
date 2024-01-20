open Asttypes
open Typedtree
open Normalized_common_ast
open Normalized_ir_ast

let const_to_aval = function
  | Const_int n -> IntV n
  | Const_string (s, _, _) -> StrV s
  | _ -> assert false

let get_bop s =
  if s = "+^" then UAdd
  else if s = "-^" then USub
  else if s = "*^" then UMul
  else if s = "/^" then UDiv
  else if s = "+" then SAdd
  else if s = "-" then SSub
  else if s = "*" then SMul
  else if s = "/" then SDiv
  else assert false

let pdot_to_aval p s =
  match p with
  | Path.Pdot (Path.Pident id, "Hashtbl") ->
      if Ident.name id = "Stdlib" then
        if s = "replace" then HashReplace
        else if s = "find" then HashFind
        else assert false
      else assert false
  | Path.Pdot (Path.Pident id, "Primitives") ->
      if Ident.name id = "OCamYul" then
        if s = "caller" then Caller else Bop (get_bop s)
      else assert false
  (* | Path.Pident id ->
      if Ident.name id = "Stdlib" then Bop (get_bop s) else assert false *)
  | _ -> assert false

let rec normalize_aux { exp_desc = e; exp_type = t; _ } k =
  match e with
  | Texp_ident (Pident s, _, _) -> (AVal (Var (Ident.unique_name s)), t) |> k
  | Texp_ident (Pdot (p, s), _, _) -> (AVal (pdot_to_aval p s), t) |> k
  | Texp_constant x -> (AVal (const_to_aval x), t) |> k
  | Texp_construct (_, { Types.cstr_name = s; _ }, []) ->
      ( AVal
          (if s = "true" then BoolV true
           else if s = "false" then BoolV false
           else if s = "()" then UnitV
           else assert false),
        t )
      |> k
  | Texp_construct (_, { Types.cstr_name = s; _ }, [ { exp_desc = e; _ } ]) ->
      let n =
        match e with Texp_constant (Const_int n) -> n | _ -> assert false
      in
      ( AVal
          (if s = "UInt" then (
             assert (n >= 0);
             IntV n)
           else if s = "SInt" then IntV n
           else assert false),
        t )
      |> k
  | Texp_apply (f, args) ->
      let args = List.filter_map (fun x -> match x with _, y -> y) args in
      normalize_name f (fun e1' ->
          let rec args_aux acc = function
            | [] -> (AApp (e1', List.rev acc, t), t) |> k
            | x :: xs -> normalize_name x (fun x' -> args_aux (x' :: acc) xs)
          in
          args_aux [] args)
  | Texp_sequence (e1, e2) ->
      normalize_aux e1 (fun (e1', _) -> ASeq (e1', normalize_aux e2 k))
  | Texp_let (_, [ { vb_pat = p; vb_expr = e1; _ } ], e2) ->
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
          match Utils.count_vars_in_type t with
          | Some xs -> ALetin (([ var ], [ (var, xs) ]), e', Var var |> k)
          | None -> ALetin (([ var ], []), e', Var var |> k)))

let normalize e = normalize_aux e (fun (x, _) -> ACexp x)
