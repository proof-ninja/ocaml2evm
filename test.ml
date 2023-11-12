module SimpleStrage : sig
  type storage

  val set : int -> storage -> unit * storage
  val get : unit -> storage -> int * storage
  val incr : unit -> storage -> unit * storage
  val twice : int -> storage -> int * storage
end = struct
  type storage = int
  type hoge = { unko : int; unko2 : string }

  let set n s = ((), n)
  let get n s = (s, s)
  let incr n s = ((), s + 1)
  let twice n s = (n + n, s)
end
