open Normalized_common_ast

type acexp =
  | AVal of value
  | AApp of (value * value list * Types.type_expr)
  | ATuple of value list
  | AIf of value * aexp * aexp

and aexp =
  | ACexp of acexp
  | ASeq of acexp * aexp
  | ALetin of (string list * (string * string list) list) * acexp * aexp

type adecl = {
  name : Ident.t;
  arg_pats : Typedtree.pattern list;
  body : aexp;
  mutability : Abi.state_mutability;
}

let rec string_of_acexp = function
  | AVal v -> string_of_value v
  | AApp (f, args, _) ->
      string_of_value f
      ^ List.fold_left (fun acc x -> acc ^ " " ^ string_of_value x) "" args
  | ATuple vs ->
      "("
      ^ List.fold_left (fun acc x -> acc ^ ", " ^ string_of_value x) "" vs
      ^ ")"
  | AIf (e1, e2, e3) -> "if " ^ string_of_value e1 ^ " then " ^ string_of_aexp e2 ^ " else " ^ string_of_aexp e3

and string_of_aexp = function
  | ACexp e -> string_of_acexp e
  | ASeq (e1, e2) -> string_of_acexp e1 ^ "; " ^ string_of_aexp e2
  | ALetin ((xs, _), e1, e2) ->
      "let "
      ^ List.fold_left (fun acc x -> acc ^ ", " ^ x) "" xs
      ^ " = " ^ string_of_acexp e1 ^ " in " ^ string_of_aexp e2

let string_of_adecl { name = id; body = e; mutability = mut; _ } =
  "let-"
  ^ Abi.string_of_mutability mut
  ^ " " ^ Ident.unique_name id ^ " [args] = " ^ string_of_aexp e
