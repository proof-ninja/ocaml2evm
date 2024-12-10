type address = string
type uint = UInt of int
type sint = SInt of int


let caller () = assert false

(* binary operators for unsigned int *)
let ( +^ ) (UInt x) (UInt y) = UInt (x + y)
let ( -^ ) (UInt x) (UInt y) = UInt (x - y)
let ( *^ ) (UInt x) (UInt y) = UInt (x * y)
let ( /^ ) (UInt x) (UInt y) = UInt (x / y)

(* binary operators for signed int *)
let ( + ) (SInt x) (SInt y) = SInt (x + y)
let ( - ) (SInt x) (SInt y) = SInt (x - y)
let ( * ) (SInt x) (SInt y) = SInt (x * y)
let ( / ) (SInt x) (SInt y) = SInt (x / y)

(* operations for boolean *)
(* let ( && ) b1 b2 = b1 && b2
let ( || ) b1 b2 = b1 || b2
let not b = not b
let (==) b1 b2 = b1 == b2
let (!=) b1 b2 = not (b1 == b2) *)