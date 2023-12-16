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

val fresh_var : unit -> string
val normalize : expression -> (label * label list) list -> pexp
val string_of_pexp : pexp -> string
