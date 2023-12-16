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

let string_of_abi_type = function Func -> "function"

let string_of_param_type = function
  | Uint256 -> "uint256"
  | Address -> "address"

let string_of_mutability = function
  | Pure -> "pure"
  | View -> "view"
  | Nonpayable -> "nonpayable"
  | Payable -> "payable"

let signature_of_function { abi_name = func_name; abi_inputs = inputs; _ } =
  let rec aux f = function
    | [] -> ""
    | [ { input_type = x; _ } ] -> f x
    | { input_type = x; _ } :: xs -> f x ^ "," ^ aux f xs
  in
  func_name ^ "(" ^ aux string_of_param_type inputs ^ ")"

let json_of_input { input_name = name; input_type = type_name } : Yojson.Basic.t
    =
  `Assoc
    [
      ("name", `String name); ("type", `String (string_of_param_type type_name));
    ]

let json_of_output { output_name = name; output_type = type_name } :
    Yojson.Basic.t =
  `Assoc
    [
      ("name", `String name); ("type", `String (string_of_param_type type_name));
    ]

let json_of_abi
    {
      abi_name = name;
      abi_type = type_name;
      abi_inputs = inputs;
      abi_outputs = outputs;
      abi_mutability = mutability;
    } : Yojson.Basic.t =
  let name_pair = ("name", `String name) in
  let type_pair = ("type", `String (string_of_abi_type type_name)) in
  let inputs_pair = ("inputs", `List (List.map json_of_input inputs)) in
  let outputs_pair = ("outputs", `List (List.map json_of_output outputs)) in
  let mutability_pair =
    ("stateMutability", `String (string_of_mutability mutability))
  in
  `Assoc [ name_pair; type_pair; inputs_pair; outputs_pair; mutability_pair ]

let json_of_abis abis : Yojson.Basic.t =
  `Assoc [ ("abi", `List (List.map json_of_abi abis)) ]
