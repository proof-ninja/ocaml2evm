(*
   translating the function body to Yul code

   This function returns two values.
   The former is the result of translation.
   The latter is the variables which has the return values of the function call.
*)
val translate_function_body :
  Normalized_ast.exp -> Yul_ast.block * Yul_ast.id list
