val fresh_var : unit -> string
val count_vars_in_type : Types.type_expr -> string list option

val flatten_tuple_pat :
  Typedtree.pattern -> (string * string list) list * string list
