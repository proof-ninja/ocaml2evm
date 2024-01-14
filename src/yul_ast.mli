type block = statement list

and statement =
  | Block of block
  | FunctionDef of id * id list * id list * block
  | Let of (idlist * exp)
  | Assign of (idlist * exp)
  | If of exp * block
  | Exp of exp
  | Switch of exp * case list * default

and exp =
  | FunctionCall of (id * exp list)
  | ID of id
  | Literal of lit
  | EVM of dialect

and case = Case of (lit * block)
and default = Default of block
and idlist = id * id list
and id = string
and lit = Hex of int | Dec of int | Bool of bool | Str of strlit
and strlit = Strlit of string

and dialect =
  | Add of (exp * exp)
  | Sub of (exp * exp)
  | Mul of (exp * exp)
  | Div of (exp * exp)
  | Not of exp
  | Lt of (exp * exp)
  | Gt of (exp * exp)
  | Eq of (exp * exp)
  | And of (exp * exp)
  | Or of (exp * exp)
  | Shr of (exp * exp)
  | Keccak256 of (exp * exp)
  | Mload of exp
  | Mstore of (exp * exp)
  | Sload of exp
  | Sstore of (exp * exp)
  | Caller
  | Callvalue
  | Calldataload of exp
  | Datasize of strlit
  | Dataoffset of strlit
  | Datacopy of (exp * exp * exp)
  | Return of (exp * exp)
  | Revert of (exp * exp)

and obj = Object of (strlit * code * obj option)
and code = Code of block

val string_of_yul : obj -> string
val json_string_of_yul : obj -> string

(* names of default runtime *)
val runtime : strlit

(* deploy code part *)
val deploy_code : code

(* names of default functions *)
val return_uint : id
val return_true : id
val return_unit : id
val get_storage : id
val set_storage : id
val get_hash_slot : id
val selector : id
val decode_as_uint : id
val decode_as_address : id

(* definitions of default functions *)
val gen_return_uint_name : int -> id
val return_uint_def : int -> statement
val return_true_def : statement
val return_unit_def : statement
val get_storage_def : int -> statement
val set_storage_def : int -> statement
val get_hash_slot_def : statement
val selector_def : statement
val decode_as_uint_def : statement
val decode_as_address_def : statement
val default_revert_def : default
val default_function_defs : block
