open Normalized_common_ast

type letexp = LVal of value | LApp of (value * value list)
type resexp = RVal of value | RTuple of value list

type exp =
  | Rexp of resexp
  | Seq of letexp * exp
  | Letin of string list * letexp * exp

val string_of_letexp : letexp -> string
val string_of_resexp : resexp -> string
val string_of_exp : exp -> string
