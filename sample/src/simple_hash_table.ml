module SimpleHash : sig
  type storage
  type mut_storage

  val set : int * int -> storage -> mut_storage -> unit * storage
  val get : int -> storage -> mut_storage -> int * storage
end = struct
  type storage = unit
  type mut_storage = (int, int) Hashtbl.t

  let set (x, y) () h =
    Hashtbl.add h x y;
    ((), ())

  let get x () h = (Hashtbl.find h x, ())
end
