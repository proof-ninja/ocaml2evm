(**
   translating the function declaration to Yul code

   This function returns two values.
   The former is the result of translation.
   The latter is the pair of the function name and its state mutability.
*)

val translate_function :
  Normalized_ast.decl -> Yul_ast.statement * (string * Abi.state_mutability)
