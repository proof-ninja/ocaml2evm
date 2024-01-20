open OCamYul.Primitives

module SimpleHash : sig
  type storage
  type mut_storage

  val set : address * uint -> storage -> mut_storage -> unit * storage
  val get : address -> storage -> mut_storage -> uint * storage
  val set_caller : uint -> storage -> mut_storage -> unit * storage
end = struct
  type storage = unit
  type mut_storage = (address, uint) Hashtbl.t

  let set (x, y) () h =
    Hashtbl.replace h x y;
    ((), ())

  let get x () h = (Hashtbl.find h x, ())

  let set_caller x () h =
    Hashtbl.replace h (caller ()) x;
    ((), ())
end
