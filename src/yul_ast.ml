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

and lit =
  (* Hex of int *)
  | Hex of string
  | Dec of int
  | Bool of bool
  | Str of strlit

and strlit = Strlit of string

and dialect =
  | Add of (exp * exp)
  | Sub of (exp * exp)
  | Mul of (exp * exp)
  | Div of (exp * exp)
  | SDiv of (exp * exp)
  | Not of exp
  | Lt of (exp * exp)
  | Gt of (exp * exp)
  | SLt of (exp * exp)
  | SGt of (exp * exp)
  | Eq of (exp * exp)
  | And of (exp * exp)
  | Or of (exp * exp)
  | Iszero of exp
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

let hex_of_int n =
  (* "0x" ^ *)
  Hex
    (let hex = Printf.sprintf "%x" n in
     if n >= 0 then hex
     else
       let h =
         String.get hex 0 |> Char.code |> ( + ) 8
         |> (fun x ->
              let diff = x - Char.code '9' - 1 in
              if diff >= 0 then Char.code 'a' |> ( + ) diff |> Char.chr
              else Char.chr x)
         |> Char.escaped
       in
       String.make 48 'f' ^ h ^ String.sub hex 1 (String.length hex - 1))

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
  | FunctionDef (func_name, args, rets, body) ->
      let rets =
        match rets with
        | [] -> ""
        | _ -> "-> " ^ string_of_args (fun x -> x) rets ^ " "
      in
      indent_depth_to_indent n ^ "function " ^ func_name ^ " ("
      ^ string_of_args (fun x -> x) args
      ^ ") " ^ rets ^ string_of_yul_block n body
  | Let (vars, e) ->
      "let " ^ string_of_idlist vars (fun x -> x) ^ " := " ^ string_of_yul_exp e
  | Assign (vars, e) ->
      string_of_idlist vars (fun x -> x) ^ " := " ^ string_of_yul_exp e
  | If (e, b) -> "if " ^ string_of_yul_exp e ^ " " ^ string_of_yul_block n b
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
  | Hex x -> (* hex_of_int x *) "0x" ^ x
  | Dec x -> string_of_int x
  | Bool x -> string_of_bool x
  | Str x -> string_of_strlit x

and string_of_strlit = function Strlit x -> "\"" ^ x ^ "\""

and string_of_yul_dialect = function
  | Add x -> "add" ^ string_of_dialect_arg_2 x
  | Sub x -> "sub" ^ string_of_dialect_arg_2 x
  | Mul x -> "mul" ^ string_of_dialect_arg_2 x
  | Div x -> "div" ^ string_of_dialect_arg_2 x
  | SDiv x -> "sdiv" ^ string_of_dialect_arg_2 x
  | Not x -> "not" ^ string_of_dialect_arg_1 x
  | Lt x -> "lt" ^ string_of_dialect_arg_2 x
  | Gt x -> "gt" ^ string_of_dialect_arg_2 x
  | SLt x -> "slt" ^ string_of_dialect_arg_2 x
  | SGt x -> "sgt" ^ string_of_dialect_arg_2 x
  | Eq x -> "eq" ^ string_of_dialect_arg_2 x
  | And x -> "and" ^ string_of_dialect_arg_2 x
  | Or x -> "or" ^ string_of_dialect_arg_2 x
  | Iszero x -> "iszero" ^ string_of_dialect_arg_1 x
  | Shr x -> "shr" ^ string_of_dialect_arg_2 x
  | Keccak256 x -> "keccak256" ^ string_of_dialect_arg_2 x
  | Mload x -> "mload" ^ string_of_dialect_arg_1 x
  | Mstore x -> "mstore" ^ string_of_dialect_arg_2 x
  | Sload x -> "sload" ^ string_of_dialect_arg_1 x
  | Sstore x -> "sstore" ^ string_of_dialect_arg_2 x
  | Caller -> "caller" ^ "()"
  | Callvalue -> "callvalue" ^ "()"
  | Calldataload x -> "calldataload" ^ string_of_dialect_arg_1 x
  | Datasize x -> "datasize" ^ "(" ^ string_of_strlit x ^ ")"
  | Dataoffset x -> "dataoffset" ^ "(" ^ string_of_strlit x ^ ")"
  | Datacopy x -> "datacopy" ^ string_of_dialect_arg_3 x
  | Return x -> "return" ^ string_of_dialect_arg_2 x
  | Revert x -> "revert" ^ string_of_dialect_arg_2 x

and string_of_dialect_arg_1 x = "(" ^ string_of_yul_exp x ^ ")"

and string_of_dialect_arg_2 (x, y) =
  "(" ^ string_of_yul_exp x ^ ", " ^ string_of_yul_exp y ^ ")"

and string_of_dialect_arg_3 (x, y, z) =
  "(" ^ string_of_yul_exp x ^ ", " ^ string_of_yul_exp y ^ ", "
  ^ string_of_yul_exp z ^ ")"

and string_of_yul_code n = function
  | Code b -> "code " ^ string_of_yul_block n b

and string_of_yul_object n = function
  | Object (name, c, x) -> (
      let common =
        "object " ^ string_of_strlit name ^ " {"
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

let json_string_of_yul x =
  String.map (fun x -> if x = '\n' then ' ' else x) (string_of_yul x)

(* names of default runtime *)
let runtime = Strlit "runtime"

(* deploy code part *)
let deploy_code =
  Code
    [
      Exp (EVM (Sstore (Literal (Dec 0), EVM Caller)));
      Exp
        (EVM
           (Datacopy
              (Literal (Dec 0), EVM (Dataoffset runtime), EVM (Datasize runtime))));
      Exp (EVM (Return (Literal (Dec 0), EVM (Datasize runtime))));
    ]

(* names of default functions *)
let return_true = "returnTrue"
let return_unit = "returnUnit"
let get_storage = "getStorage"
let set_storage = "setStorage"
let get_hash_slot = "getHashSlot"
let selector = "selector"
let decode_as_uint = "decodeAsUint"
let decode_as_address = "decodeAsAddress"
let uint_add = "uintAdd"
let uint_sub = "uintSub"
let uint_mul = "uintMul"
let uint_div = "uintDiv"
let sint_add = "sintAdd"
let sint_sub = "sintSub"
let sint_mul = "sintMul"
let sint_div = "sintDiv"

(* definitions of default functions *)
let gen_return_uint_name n = "returnUint_" ^ string_of_int n

let return_uint_def n =
  let rec gen_arg_vars n =
    if n = 0 then []
    else ("arg_" ^ string_of_int (n - 1), n - 1) :: gen_arg_vars (n - 1)
  in
  let num_args = List.rev (gen_arg_vars n) in
  let rec gen_store_vars args =
    match args with
    | [] -> []
    | (x, n) :: tl ->
        Exp (EVM (Mstore (Literal (Dec (n * 32)), ID x))) :: gen_store_vars tl
  in
  let args = fst (List.split num_args) in
  let assigns =
    gen_store_vars num_args
    @ [ Exp (EVM (Return (Literal (Dec 0), Literal (hex_of_int (32 * n))))) ]
  in
  FunctionDef (gen_return_uint_name n, args, [], assigns)

let return_true_def =
  FunctionDef
    ( return_true,
      [],
      [],
      [
        Exp (EVM (Mstore (Literal (Dec 0), Literal (Dec 1))));
        Exp (EVM (Return (Literal (Dec 0), Literal (hex_of_int 32))));
      ] )

let return_unit_def =
  FunctionDef
    ( return_unit,
      [],
      [],
      [ Exp (EVM (Return (Literal (Dec 0), Literal (hex_of_int 0)))) ] )

let get_storage_def n =
  let rec gen_ret_vars n =
    if n = 0 then []
    else ("ret_" ^ string_of_int (n - 1), n) :: gen_ret_vars (n - 1)
  in
  let ret_num_args = List.rev (gen_ret_vars n) in
  let rec gen_assign_vars args =
    match args with
    | [] -> []
    | (x, n) :: tl ->
        Assign ((x, []), EVM (Sload (Literal (Dec n)))) :: gen_assign_vars tl
  in
  let ret_args = fst (List.split ret_num_args) in
  let assigns = gen_assign_vars ret_num_args in
  FunctionDef (get_storage, [], ret_args, assigns)

let set_storage_def n =
  let rec gen_arg_vars n =
    if n = 0 then []
    else ("arg_" ^ string_of_int (n - 1), n) :: gen_arg_vars (n - 1)
  in
  let num_args = List.rev (gen_arg_vars n) in
  let rec gen_store_vars args =
    match args with
    | [] -> []
    | (x, n) :: tl ->
        Exp (EVM (Sstore (Literal (Dec n), ID x))) :: gen_store_vars tl
  in
  let args = fst (List.split num_args) in
  let assigns = gen_store_vars num_args in
  FunctionDef (set_storage, args, [], assigns)

let get_hash_slot_def =
  let slot = "$slot" in
  let key = "$key" in
  let data_slot = "$dataSlot" in
  FunctionDef
    ( get_hash_slot,
      [ slot; key ],
      [ data_slot ],
      [
        Exp (EVM (Mstore (Literal (hex_of_int 0), ID key)));
        Exp (EVM (Mstore (Literal (hex_of_int 32), ID slot)));
        Assign
          ( (data_slot, []),
            EVM (Keccak256 (Literal (hex_of_int 0), Literal (hex_of_int 64))) );
      ] )

let selector_def =
  let return_arg = "ret" in
  FunctionDef
    ( selector,
      [],
      [ return_arg ],
      [
        Assign
          ( (return_arg, []),
            EVM (Shr (Literal (Dec 224), EVM (Calldataload (Literal (Dec 0)))))
          );
      ] )

let decode_as_uint_def =
  let arg = "offset" in
  let return_arg = "v" in
  let pos = "pos" in
  FunctionDef
    ( decode_as_uint,
      [ arg ],
      [ return_arg ],
      [
        Let
          ( (pos, []),
            EVM
              (Add (Literal (Dec 4), EVM (Mul (ID arg, Literal (hex_of_int 32)))))
          );
        Assign ((return_arg, []), EVM (Calldataload (ID pos)));
      ] )

let decode_as_address_def =
  let arg = "offset" in
  let return_arg = "v" in
  FunctionDef
    ( decode_as_address,
      [ arg ],
      [ return_arg ],
      [ Assign ((return_arg, []), FunctionCall (decode_as_uint, [ ID arg ])) ]
    )

let default_revert_def =
  Default [ Exp (EVM (Revert (Literal (Dec 0), Literal (Dec 0)))) ]

let uint_add_def =
  let arg_x, arg_y = ("$x", "$y") in
  let return_arg = "$r" in
  FunctionDef
    ( uint_add,
      [ arg_x; arg_y ],
      [ return_arg ],
      [
        Assign ((return_arg, []), EVM (Add (ID arg_x, ID arg_y)));
        If
          ( EVM
              (Or
                 ( EVM (Gt (ID arg_x, ID return_arg)),
                   EVM (Gt (ID arg_y, ID return_arg)) )),
            [ Exp (EVM (Revert (Literal (Dec 0), Literal (Dec 3)))) ] );
      ] )

let uint_sub_def =
  let arg_x, arg_y = ("$x", "$y") in
  let return_arg = "$r" in
  FunctionDef
    ( uint_sub,
      [ arg_x; arg_y ],
      [ return_arg ],
      [
        Assign ((return_arg, []), EVM (Sub (ID arg_x, ID arg_y)));
        If
          ( EVM (Gt (ID return_arg, ID arg_x)),
            [ Exp (EVM (Revert (Literal (Dec 0), Literal (Dec 0)))) ] );
      ] )

let uint_mul_def =
  let arg_x, arg_y = ("$x", "$y") in
  let return_arg = "$r" in
  FunctionDef
    ( uint_mul,
      [ arg_x; arg_y ],
      [ return_arg ],
      [
        Assign ((return_arg, []), EVM (Mul (ID arg_x, ID arg_y)));
        If
          ( EVM
              (Iszero
                 (EVM
                    (Or
                       ( EVM (Iszero (ID arg_x)),
                         EVM
                           (Eq (ID arg_y, EVM (Div (ID return_arg, ID arg_x))))
                       )))),
            [ Exp (EVM (Revert (Literal (Dec 0), Literal (Dec 0)))) ] );
      ] )

let uint_div_def =
  let arg_x, arg_y = ("$x", "$y") in
  let return_arg = "$r" in
  FunctionDef
    ( uint_div,
      [ arg_x; arg_y ],
      [ return_arg ],
      [
        If
          ( EVM (Iszero (ID arg_y)),
            [ Exp (EVM (Revert (Literal (Dec 0), Literal (Dec 0)))) ] );
        Assign ((return_arg, []), EVM (Div (ID arg_x, ID arg_y)));
      ] )

let sint_add_def =
  let arg_x, arg_y = ("$x", "$y") in
  let return_arg = "$r" in
  FunctionDef
    ( sint_add,
      [ arg_x; arg_y ],
      [ return_arg ],
      [
        Assign ((return_arg, []), EVM (Add (ID arg_x, ID arg_y)));
        If
          ( EVM
              (Or
                 ( EVM
                     (And
                        ( EVM (Iszero (EVM (SLt (ID arg_x, Literal (Dec 0))))),
                          EVM (SLt (ID return_arg, ID arg_y)) )),
                   EVM
                     (And
                        ( EVM (SLt (ID arg_x, Literal (Dec 0))),
                          EVM (Iszero (EVM (SLt (ID return_arg, ID arg_y)))) ))
                 )),
            [ Exp (EVM (Revert (Literal (Dec 0), Literal (Dec 3)))) ] );
      ] )

let sint_sub_def =
  let arg_x, arg_y = ("$x", "$y") in
  let return_arg = "$r" in
  FunctionDef
    ( sint_sub,
      [ arg_x; arg_y ],
      [ return_arg ],
      [
        Assign ((return_arg, []), EVM (Sub (ID arg_x, ID arg_y)));
        If
          ( EVM
              (Or
                 ( EVM
                     (And
                        ( EVM (Iszero (EVM (SLt (ID arg_y, Literal (Dec 0))))),
                          EVM (SGt (ID return_arg, ID arg_x)) )),
                   EVM
                     (And
                        ( EVM (SLt (ID arg_y, Literal (Dec 0))),
                          EVM (SLt (ID return_arg, ID arg_x)) )) )),
            [ Exp (EVM (Revert (Literal (Dec 0), Literal (Dec 0)))) ] );
      ] )

let sint_mul_def =
  let arg_x, arg_y = ("$x", "$y") in
  let return_arg = "$r" in
  FunctionDef
    ( sint_mul,
      [ arg_x; arg_y ],
      [ return_arg ],
      [
        Assign ((return_arg, []), EVM (Mul (ID arg_x, ID arg_y)));
        If
          ( EVM
              (And
                 ( EVM (SLt (ID arg_x, Literal (Dec 0))),
                   EVM
                     (Eq
                        ( ID arg_y,
                          Literal
                            (Hex
                               "8000000000000000000000000000000000000000000000000000000000000000")
                        )) )),
            [ Exp (EVM (Revert (Literal (Dec 0), Literal (Dec 0)))) ] );
        If
          ( EVM
              (Iszero
                 (EVM
                    (Or
                       ( EVM (Iszero (ID arg_x)),
                         EVM
                           (Eq (ID arg_y, EVM (SDiv (ID return_arg, ID arg_x))))
                       )))),
            [ Exp (EVM (Revert (Literal (Dec 0), Literal (Dec 0)))) ] );
      ] )

let sint_div_def =
  let arg_x, arg_y = ("$x", "$y") in
  let return_arg = "$r" in
  FunctionDef
    ( sint_div,
      [ arg_x; arg_y ],
      [ return_arg ],
      [
        If
          ( EVM
              (And
                 ( EVM
                     (Eq
                        ( ID arg_x,
                          Literal
                            (Hex
                               "8000000000000000000000000000000000000000000000000000000000000000")
                        )),
                   EVM
                     (Eq (ID arg_x, EVM (Sub (Literal (Dec 0), Literal (Dec 1)))))
                 )),
            [ Exp (EVM (Revert (Literal (Dec 0), Literal (Dec 0)))) ] );
        Assign ((return_arg, []), EVM (SDiv (ID arg_x, ID arg_y)));
      ] )

(* `default_function_defs` is mutable because
   adding some functions to default happens in different phases *)

let default_function_defs =
  ref [ selector_def; decode_as_uint_def; decode_as_address_def ]

let reset_default_function_defs () =
  default_function_defs :=
    [ selector_def; decode_as_uint_def; decode_as_address_def ]

let get_default_function_defs () = !default_function_defs

let update_default_function_defs new_func =
  let defs = !default_function_defs in
  if List.mem new_func defs then ()
  else default_function_defs := new_func :: defs
