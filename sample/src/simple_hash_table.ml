open OCamYul.Primitives

module SimpleHash : sig
  type storage
  type mut_storage

  val set : address * int -> storage -> mut_storage -> unit * storage
  val get : address -> storage -> mut_storage -> int * storage
  val set_caller : int -> storage -> mut_storage -> unit * storage
end = struct
  type storage = unit
  type mut_storage = (address, int) Hashtbl.t

  let set (x, y) () h =
    Hashtbl.add h x y;
    ((), ())

  let get x () h = (Hashtbl.find h x, ())

  let set_caller x () h =
    Hashtbl.add h (caller ()) x;
    ((), ())
end
