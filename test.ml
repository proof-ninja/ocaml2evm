module SimpleStorage : sig
  type storage

  val set : int -> storage -> unit * storage
  val get : int -> storage -> int * storage
  val incr : int -> storage -> unit * storage
  val twice : int -> storage -> int * storage
end = struct
  type storage = int

  let set n s = ((), n)
  let get n s = (s, s)
  let incr n s = ((), s + 1)
  let twice n s = (n + n, s)
end
