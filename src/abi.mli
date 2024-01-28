(* types for ABI *)
type abi = {
  abi_name : string;
  abi_type : abi_type;
  abi_inputs : input list;
  abi_outputs : output list;
  abi_mutability : state_mutability;
}

and input = { input_name : string; input_type : param_type }
and output = { output_name : string; output_type : param_type }
and abi_type = Func
and param_type = Uint256 | Int256 | Address
and state_mutability = Pure | View | Nonpayable | Payable

val string_of_mutability : state_mutability -> string

(* compares and returns the stronger mutability
   (e.g., stronger_mutability View Nonpayable = Nonpayable) *)
val stronger_mutability :
  state_mutability -> state_mutability -> state_mutability

(* returns signature of function (e.g., `transfer(address,uint256)` ) *)
val signature_of_function : abi -> string
val json_of_abis : abi list -> Yojson.Basic.t
