open Normalized_common_ast



(** expressions that can be placed at the last of let-binding sequences *)
type resexp = RVal of value | RTuple of value list
(** expressions that can be placed at the right-hand side of a let-binding *)
type letexp = LVal of value | LApp of (value * value list) | LIf of value * exp * exp

and exp =
  | Rexp of resexp
  | Seq of letexp * exp
  | Letin of string list * letexp * exp
  | If of value * exp * exp

(** a function declaration with stateMutability field of ABI *)
type decl = {
  name : Ident.t;
  arg_ids : string list;
  body : exp;
  mutability : Abi.state_mutability;
}

val string_of_letexp : letexp -> string
val string_of_resexp : resexp -> string
val string_of_exp : exp -> string
val string_of_decl : decl -> string
