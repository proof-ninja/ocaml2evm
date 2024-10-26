type address
type uint = UInt of int
type sint = SInt of int
(* type bool_ = bool *)

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

(* val ( && ) : bool_ -> bool_ -> bool_
val ( || ) : bool_ -> bool_ -> bool_
val not : bool_ -> bool_
val ( == ) : bool_ -> bool_ -> bool_
val ( != ) : bool_ -> bool_ -> bool_ *)