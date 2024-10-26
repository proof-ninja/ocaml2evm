type bop = UAdd | USub | UMul | UDiv | SAdd | SSub | SMul | SDiv | And | Or | Not | Eq | Neq | Lt | Gt | Lte | Gte

type value =
  | Var of string
  | IntV of int
  | BoolV of bool
  | StrV of string
  | UnitV
  | HashReplace
  | HashFind
  | Caller
  | Bop of bop

let string_of_bop = function
  | UAdd -> "+^"
  | USub -> "-^"
  | UMul -> "*^"
  | UDiv -> "/^"
  | SAdd -> "+"
  | SSub -> "-"
  | SMul -> "*"
  | SDiv -> "/"
  | And -> "&&"
  | Or -> "||"
  | Not -> "not"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Gt -> ">"
  | Lte -> "<="
  | Gte -> ">="

let string_of_value = function
  | Var s -> s
  | IntV n -> string_of_int n
  | BoolV b -> string_of_bool b
  | StrV s -> s
  | UnitV -> "()"
  | HashReplace -> "Hashtbl.replace"
  | HashFind -> "Hashtbl.find"
  | Caller -> "caller"
  | Bop b -> "(" ^ string_of_bop b ^ ")"
