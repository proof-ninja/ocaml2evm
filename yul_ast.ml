type block = statement list

and statement =
  | Block of block
  | FunctionDef of id * id list * id option * block
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
and strlit = string

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

let hex_of_int n = Printf.sprintf "0x%x" n

let string_of_idlist (x, xs) f =
  f x ^ List.fold_left (fun x y -> x ^ ", " ^ f y) "" xs

let string_of_args f = function
  | [] -> ""
  | x :: xs -> f x ^ List.fold_left (fun x y -> x ^ ", " ^ f y) "" xs

let indent_depth_to_indent n = "\n" ^ String.make (n * 2) ' '

let rec string_of_yul_block n b =
  "{"
  ^ List.fold_left
      (fun x y ->
        x ^ indent_depth_to_indent (n + 1) ^ string_of_yul_statement (n + 1) y)
      "" b
  ^ indent_depth_to_indent n ^ "}"

and string_of_yul_statement n = function
  | Block b -> string_of_yul_block n b
  | FunctionDef (func_name, args, ret, body) ->
      let ret = match ret with Some x -> "-> " ^ x ^ " " | None -> "" in
      indent_depth_to_indent n ^ func_name ^ " ("
      ^ string_of_args (fun x -> x) args
      ^ ") " ^ ret ^ string_of_yul_block n body
  | Let (vars, e) ->
      "let " ^ string_of_idlist vars (fun x -> x) ^ " := " ^ string_of_yul_exp e
  | Assign (vars, e) ->
      string_of_idlist vars (fun x -> x) ^ " := " ^ string_of_yul_exp e
  | If (e, b) ->
      "if " ^ string_of_yul_exp e ^ " " ^ string_of_yul_block (n + 1) b
  | Exp e -> string_of_yul_exp e
  | Switch (e, cases, b) ->
      indent_depth_to_indent n ^ "switch " ^ string_of_yul_exp e
      ^ List.fold_left
          (fun x case ->
            x ^ indent_depth_to_indent n ^ string_of_yul_case n case)
          "" cases
      ^ indent_depth_to_indent n ^ string_of_yul_default n b

and string_of_yul_exp = function
  | FunctionCall (func_name, args) ->
      let args = string_of_args string_of_yul_exp args in
      func_name ^ "(" ^ args ^ ")"
  | ID x -> x
  | Literal lit -> string_of_yul_lit lit
  | EVM d -> string_of_yul_dialect d

and string_of_yul_case n = function
  | Case (arg, b) ->
      "case " ^ string_of_yul_lit arg ^ " " ^ string_of_yul_block n b

and string_of_yul_default n = function
  | Default b -> "default " ^ string_of_yul_block n b

and string_of_yul_lit = function
  | Hex x -> hex_of_int x
  | Dec x -> string_of_int x
  | Bool x -> string_of_bool x
  | Str x -> x

and string_of_yul_dialect = function
  | Add (x, y) ->
      "add(" ^ string_of_yul_exp x ^ ", " ^ string_of_yul_exp y ^ ")"
  | Sub (x, y) ->
      "sub(" ^ string_of_yul_exp x ^ ", " ^ string_of_yul_exp y ^ ")"
  | Mul (x, y) ->
      "mul(" ^ string_of_yul_exp x ^ ", " ^ string_of_yul_exp y ^ ")"
  | Div (x, y) ->
      "div(" ^ string_of_yul_exp x ^ ", " ^ string_of_yul_exp y ^ ")"
  | Not x -> "not(" ^ string_of_yul_exp x ^ ")"
  | Lt (x, y) -> "lt(" ^ string_of_yul_exp x ^ ", " ^ string_of_yul_exp y ^ ")"
  | Gt (x, y) -> "gt(" ^ string_of_yul_exp x ^ ", " ^ string_of_yul_exp y ^ ")"
  | Eq (x, y) -> "eq(" ^ string_of_yul_exp x ^ ", " ^ string_of_yul_exp y ^ ")"
  | And (x, y) ->
      "and(" ^ string_of_yul_exp x ^ ", " ^ string_of_yul_exp y ^ ")"
  | Or (x, y) -> "or(" ^ string_of_yul_exp x ^ ", " ^ string_of_yul_exp y ^ ")"
  | Shr (x, y) ->
      "shr(" ^ string_of_yul_exp x ^ ", " ^ string_of_yul_exp y ^ ")"
  | Mload x -> "mload(" ^ string_of_yul_exp x ^ ")"
  | Mstore (x, y) ->
      "mstore(" ^ string_of_yul_exp x ^ ", " ^ string_of_yul_exp y ^ ")"
  | Sload x -> "sload(" ^ string_of_yul_exp x ^ ")"
  | Sstore (x, y) ->
      "sstore(" ^ string_of_yul_exp x ^ ", " ^ string_of_yul_exp y ^ ")"
  | Caller -> "caller()"
  | Callvalue -> "callvalue()"
  | Calldataload x -> "add(" ^ string_of_yul_exp x ^ ")"
  | Datasize x -> "datasize(\"" ^ x ^ "\")"
  | Dataoffset x -> "dataoffset(\"" ^ x ^ "\")"
  | Datacopy (x, y, z) ->
      "datacopy(" ^ string_of_yul_exp x ^ ", " ^ string_of_yul_exp y ^ ", "
      ^ string_of_yul_exp z ^ ")"
  | Return (x, y) ->
      "return(" ^ string_of_yul_exp x ^ ", " ^ string_of_yul_exp y ^ ")"
  | Revert (x, y) ->
      "revert(" ^ string_of_yul_exp x ^ ", " ^ string_of_yul_exp y ^ ")"

and string_of_yul_code n = function
  | Code b -> "code " ^ string_of_yul_block n b

and string_of_yul_object n = function
  | Object (name, c, x) -> (
      let common =
        "object \"" ^ name ^ "\" "
        ^ indent_depth_to_indent (n + 1)
        ^ string_of_yul_code (n + 1) c
      in
      match x with
      | Some obj ->
          common
          ^ indent_depth_to_indent (n + 1)
          ^ indent_depth_to_indent (n + 1)
          ^ string_of_yul_object (n + 1) obj
          ^ indent_depth_to_indent n ^ "}"
      | None -> common ^ indent_depth_to_indent n ^ "}")

let string_of_yul obj = string_of_yul_object 0 obj
