module SimpleStorage : sig
  type storage

  val set : int -> storage -> unit * storage
  val get : unit -> storage -> int * storage
  val incr : unit -> storage -> unit * storage
  val twice : int -> storage -> int * storage
  val add : int * int -> storage -> int * storage
  val sub : int * int -> storage -> int * storage
  val mul : int * int -> storage -> int * storage
  val div : int * int -> storage -> int * storage
  val anormaltest : int -> storage -> int * storage
end = struct
  type storage = int

  let set n _ = ((), n)
  let get () s = (s, s)
  let incr () s = ((), s + 1)
  let twice n s = (n * 2, s)
  let add (x, y) s = (x + y, s)
  let sub (x, y) s = (x - y, s)
  let mul (x, y) s = (x * y, s)
  let div (x, y) s = (x / y, s)

  let anormaltest n s =
    let m =
      let l = s * 2 in
      let o = n * 3 in
      l + o
    in
    (m, m)
end
