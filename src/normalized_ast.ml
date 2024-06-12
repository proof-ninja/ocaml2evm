open Normalized_common_ast

exception Whoo of int

type resexp = RVal of value | RTuple of value list

type letexp = LVal of value | LApp of (value * value list) | LIf of value * exp * exp

and exp =
  | Rexp of resexp
  | Seq of letexp * exp
  | Letin of string list * letexp * exp
  | If of value * exp * exp


type decl = {
  name : Ident.t;
  arg_ids : string list;
  body : exp;
  mutability : Abi.state_mutability;
}

let string_of_resexp = function
  | RVal v -> string_of_value v
  | RTuple vs ->
      "("
      ^ List.fold_left (fun acc x -> acc ^ ", " ^ string_of_value x) "" vs
      ^ ")"

let rec string_of_letexp = function
  | LVal v -> string_of_value v
  | LApp (f, xs) ->
      string_of_value f
      ^ List.fold_left (fun acc x -> acc ^ " " ^ string_of_value x) "" xs
  | LIf (v, e1, e2) -> "if " ^ string_of_value v ^ " then " ^ string_of_exp e1 ^ " else " ^ string_of_exp e2
and string_of_exp e =
  match e with
  | Rexp e -> string_of_resexp e
  | Seq (e1, e2) -> string_of_letexp e1 ^ "; " ^ string_of_exp e2
  | Letin (vars, e1, e2) ->
      "let"
      ^ List.fold_left (fun acc x -> acc ^ ", " ^ x) "" vars
      ^ " = " ^ string_of_letexp e1 ^ " in " ^ string_of_exp e2
  | If (v, e1, e2) -> "if " ^ string_of_value v ^ " then " ^ string_of_exp e1 ^ " else " ^ string_of_exp e2

  

let string_of_decl
    { name = func_name; arg_ids = args; body = e; mutability = mut } =
  "let-"
  ^ Abi.string_of_mutability mut
  ^ " "
  ^ Ident.unique_name func_name
  ^ List.fold_left (fun acc x -> acc ^ " " ^ x) "" args
  ^ " = " ^ string_of_exp e
