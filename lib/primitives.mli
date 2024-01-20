type address
type uint = UInt of int
type sint = SInt of int

val caller : unit -> address

(* binary operators for unsigned int *)
val ( +^ ) : uint -> uint -> uint
val ( -^ ) : uint -> uint -> uint
val ( *^ ) : uint -> uint -> uint
val ( /^ ) : uint -> uint -> uint

(* binary operators for signed int *)
val ( + ) : sint -> sint -> sint
val ( - ) : sint -> sint -> sint
val ( * ) : sint -> sint -> sint
val ( / ) : sint -> sint -> sint
