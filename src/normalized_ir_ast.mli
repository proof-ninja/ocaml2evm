open Normalized_common_ast

type acexp =
  | AVal of value
  | AApp of (value * value list * Types.type_expr)
  | ATuple of value list

type aexp =
  | ACexp of acexp
  | ASeq of acexp * aexp
  | ALetin of (string list * (string * string list) list) * acexp * aexp

val string_of_acexp : acexp -> string
val string_of_aexp : aexp -> string
