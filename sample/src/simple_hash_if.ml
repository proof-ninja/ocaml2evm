open OCamYul.Primitives

module SimpleHashIf : sig
  type storage
  type mut_storage

  val set : address * uint -> storage -> mut_storage -> unit * storage
  val get : address -> storage -> mut_storage -> uint * storage
  val set_caller : uint -> storage -> mut_storage -> unit * storage
end = struct
  type storage = unit
  type mut_storage = (address, uint) Hashtbl.t

  let set (x, y) () h =
    (if true then let a = UInt 2 in let b = UInt 3 in let c = a +^ b in Hashtbl.replace h x y else ()); let a = if false then SInt 1 else SInt 2 in let b = SInt 2 in let c = a + b in 
    ((), ())

  let get x () h = 
    let a = 
      if let c = true in c 
        then SInt 1 
        else let b = if false 
          then SInt 3
          else SInt 4 in b + b
    in (Hashtbl.find h x, ())


  let set_caller x () h =
    Hashtbl.replace h (caller ()) x;
    ((), ())
end
