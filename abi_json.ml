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
and param_type = Uint256 | Address
and state_mutability = Pure | View | Nonpayable | Payable

let string_of_abi_type = function Func -> "\"function\""

let string_of_param_type = function
  | Uint256 -> "\"uint256\""
  | Address -> "\"address\""

let string_of_mutability = function
  | Pure -> "\"pure\""
  | View -> "\"view\""
  | Nonpayable -> "\"nonpayable\""
  | Payable -> "\"payable\""

let string_of_inputs = function
  | [] -> "[]"
  | x :: xs ->
      let aux { input_name = name; input_type = t } =
        "{\n  " ^ "\"name\": \"" ^ name ^ "\",\n  " ^ "\"type\": "
        ^ string_of_param_type t ^ "\n}"
      in
      "[\n  " ^ List.fold_left (fun y z -> y ^ ",\n" ^ aux z) (aux x) xs ^ "\n]"

let string_of_outputs = function
  | [] -> "[]"
  | x :: xs ->
      let aux { output_name = name; output_type = t } =
        "{\n  " ^ "\"name\": \"" ^ name ^ "\",\n  " ^ "\"type\": "
        ^ string_of_param_type t ^ "\n}"
      in
      List.fold_left (fun y z -> y ^ ",\n" ^ aux z) (aux x) xs

let string_of_abi = function
  | {
      abi_name = name;
      abi_type = t;
      abi_inputs = inputs;
      abi_outputs = outputs;
      abi_mutability = mutes;
    } ->
      "{\n  " ^ "\"name\": \"" ^ name ^ "\",\n  " ^ "\"type\": "
      ^ string_of_abi_type t ^ ",\n  " ^ "\"inputs\": "
      ^ string_of_inputs inputs ^ ",\n  " ^ "\"outputs\": "
      ^ string_of_outputs outputs ^ ",\n  " ^ "\"stateMutability\": "
      ^ string_of_mutability mutes ^ "\n}"
