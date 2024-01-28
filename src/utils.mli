(* generates a fresh identifier *)
val fresh_var : unit -> string

(* takes a type and if the type is tuple, returns identifier list for each elements of a tuple type *)
val count_vars_in_type : Types.type_expr -> string list option

(*
   takes the pattern and returns the pair

   The first element is variable renaming mappings.
   The second element is the variables of flattened tuple.
   For example, if the returned value is `[(a, [b; c; d]); (x, [y; z])], [y; z; w; b; c; d]`,
   the variable `a` has a 3-tuple type and is replaced with `(b, c, d)` and
   the whole result of flattening is `(y, z, w, b, c, d)` where `w` is generated from `_`.
*)
val flatten_tuple_pat :
  Typedtree.pattern -> (string * string list) list * string list

(* checks whether the argument pattern is the same as the returned value *)
val same_pat_exp : Typedtree.pattern -> Typedtree.expression -> bool
