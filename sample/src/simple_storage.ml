open OCamYul.Primitives

module SimpleStorage : sig
  type storage

  val set : sint -> storage -> unit * storage
  val get : unit -> storage -> sint * storage
  val incr : unit -> storage -> unit * storage
  val decr : unit -> storage -> unit * storage
  val twice : sint -> storage -> sint * storage
  val add : sint * sint -> storage -> sint * storage
  val sub : sint * sint -> storage -> sint * storage
  val mul : sint * sint -> storage -> sint * storage
  val div : sint * sint -> storage -> sint * storage
  val anormaltest : sint -> storage -> sint * storage
end = struct
  type storage = sint

  let set n _ = ((), n)
  let get () s = (s, s)
  let incr () s = ((), s + SInt 1)
  let decr () s = ((), s + SInt (-1))
  let twice n s = (n * SInt 2, s)
  let add (x, y) s = (x + y, s)
  let sub (x, y) s = (x - y, s)
  let mul (x, y) s = (x * y, s)
  let div (x, y) s = (x / y, s)

  let anormaltest n s =
    let m =
      let l = s * SInt 2 in
      let o = n * SInt 3 in
      l + o
    in
    (m, m)
end
