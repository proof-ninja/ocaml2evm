open Asttypes
open Typedtree
open Normalized_common_ast
open Normalized_ir_ast

let const_to_aval = function
  | Const_int n -> IntV n
  | Const_string (s, _, _) -> StrV s
  | _ -> assert false

let get_bop s =
  match s with
    | "+^" -> UAdd
    | "-^" -> USub
    | "*^" -> UMul
    | "/^" -> UDiv
    | "+" -> SAdd
    | "-" -> SSub
    | "*" -> SMul
    | "/" -> SDiv
    | "&&" -> And
    | "||" -> Or
    | "not" -> Not
    | "=" -> Eq
    | "!=" -> Neq
    | "<" -> Lt
    | ">" -> Gt
    | "<=" -> Lte
    | ">=" -> Gte
    | _ -> assert false
  (*if s = "+^" then UAdd
  else if s = "-^" then USub
  else if s = "*^" then UMul
  else if s = "/^" then UDiv
  else if s = "+" then SAdd
  else if s = "-" then SSub
  else if s = "*" then SMul
  else if s = "/" then SDiv
  else assert false*)

(* operation functions made usable especially, such as replace caller*)
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
  | Path.Pident id -> if Ident.name id = "Stdlib" then Bop (get_bop s) else assert false
  | _ -> assert false

  (* The first argument p is a storage. To check whether the storage changes, it is needed. 
     The last argument k is a continuation. A first argument of k is hole, and a first element of a return value is AST with the hole.*)
let rec normalize_aux p { exp_desc = e; exp_type = t; _ } k :aexp * bool=
  match e with
  | Texp_ident (Pident s, _, _) ->
      (AVal (Var (Ident.unique_name s)), t, false) |> k
  | Texp_ident (Pdot (p, s), _, _) -> (AVal (pdot_to_aval p s), t, false) |> k
  | Texp_constant x -> (AVal (const_to_aval x), t, false) |> k
  | Texp_construct (_, { Types.cstr_name = s; _ }, []) ->
      ( AVal
          (if s = "true" then BoolV true
           else if s = "false" then BoolV false
           else if s = "()" then UnitV
           else assert false),
        t,
        false )
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
        t,
        false )
      |> k
  | Texp_apply (f, args) ->
      let args = List.filter_map (fun x -> match x with _, y -> y) args in
      normalize_name f (fun e1' ->
          let rec args_aux acc = function
            | [] -> (AApp (e1', List.rev acc, t), t, false) |> k
            | x :: xs -> normalize_name x (fun x' -> args_aux (x' :: acc) xs)
          in
          args_aux [] args)
  | Texp_sequence (e1, e2) ->
      normalize_aux None e1 (fun (e1', _, _) ->
          let e, non_update_stor = normalize_aux p e2 k in
          (ASeq (e1', e), non_update_stor))
  | Texp_let (_, [ { vb_pat = let_pat; vb_expr = e1; _ } ], e2) ->
      let rename, vars = Utils.flatten_tuple_pat let_pat in
      normalize_aux None e1 (fun (e1', _, _) ->
          let e, non_update_stor = normalize_aux p e2 k in
          (ALetin ((vars, rename), e1', e), non_update_stor))
  | Texp_tuple el ->
      let non_update_stor =
        match p with
        | Some p -> (
            match el with [ _; e ] -> Utils.same_pat_exp p e | _ -> false)
        | None -> false
      in
      let rec tuple_aux acc = function
        | [] -> (ATuple (List.rev acc), t, non_update_stor) |> k
        | x :: xs -> normalize_name x (fun x' -> tuple_aux (x' :: acc) xs)
      in
      tuple_aux [] el
  | Texp_match
      ( e1,
        [ { c_lhs = { pat_desc = Tpat_value match_pat; _ }; c_rhs = e2; _ } ],
        _ ) ->
      let rename, vars = Utils.flatten_tuple_pat (match_pat :> pattern) in
      normalize_aux None e1 (fun (e1', _, _) ->
          let e, non_update_stor = normalize_aux p e2 k in
          (ALetin ((vars, rename), e1', e), non_update_stor))
  | Texp_ifthenelse (e1, e2, e3) -> 
    let a, b = normalize_aux p e2 (fun (x, _, b) -> (ACexp x, b)) in
    let e3' = match e3 with Some e -> e | _ -> assert false in
    let a2, b2 = normalize_aux p e3' (fun (x, _, b) -> (ACexp x, b)) in
    let a, b = normalize_name e1 (fun x -> k (AIf(x, a, a2), t, b && b2 )) in
    a, b
  | _ -> assert false

(* when a new variable is needed *)
and normalize_name e k =
  normalize_aux None e (fun (e', t, _) ->
      match e' with
      | AVal v -> k v
      | _ -> (
          let var = Utils.fresh_var () in
          let e, non_update_stor = k (Var var) in
          match Utils.count_vars_in_type t with
          | Some xs ->
              (ALetin (([ var ], [ (var, xs) ]), e', e), non_update_stor)
          | None -> (ALetin (([ var ], []), e', e), non_update_stor)))


(* because a second argument is a storage and we have to check whether it is possible to change (for nonpayable/view),
   the storage argument is distinct as stor_pat*)
let rec expand_function_decl n stor_pat e =
  match e with
  | {
   exp_desc =
     Texp_function { cases = { c_lhs = arg_pat; c_rhs = body; _ } :: []; _ };
   _;
  } ->
      let args, body, non_update_stor =
        if n = 1 then expand_function_decl (n + 1) (Some arg_pat) body
        else expand_function_decl (n + 1) stor_pat body
      in
      (arg_pat :: args, body, non_update_stor)
  | _ ->
      let e, non_update_stor =
        normalize_aux stor_pat e (fun (x, _, b) -> (ACexp x, b))
      in
      ([], e, non_update_stor)

let normalize { vb_pat = p; vb_expr = e; _ } =
  match p with
  | { pat_desc = Tpat_var (func_name, _); _ } ->
      let args, e, non_update_stor = expand_function_decl 0 None e in
      {
        name = func_name;
        arg_pats = args;
        body = e;
        mutability = (if non_update_stor then Abi.View else Abi.Nonpayable);
      }
  | _ -> assert false
