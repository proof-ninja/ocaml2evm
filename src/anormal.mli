open Asttypes
open Typedtree

type bop = Add | Sub | Mul | Div

type aval =
  | Var of string
  | IntV of int
  | BoolV of bool
  | StrV of string
  | UnitV
  | HashAdd
  | HashFind
  | Caller
  | Bop of bop

type letexp = LVal of aval | LApp of (aval * aval list)
type resexp = RVal of aval | RTuple of aval list

type pexp =
  | Rexp of resexp
  | Seq of letexp * pexp
  | Letin of string list * letexp * pexp

val normalize : expression -> (label * label list) list -> pexp
val string_of_pexp : pexp -> string
