(*
   The first argument is used to rewrite some variables.
   For example, if the first argument includes `(x, [y; z])`,
   then all occurence of `x` in the second argument change to `y` and `z`.
   This renaming is necessary because OCaml allows the variables of tuple type
   but Yul does not.
*)
val normalize :
  (string * string list) list -> Normalized_ir_ast.aexp -> Normalized_ast.exp
