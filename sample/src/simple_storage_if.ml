open OCamYul.Primitives

module SimpleStorageIf : sig
  type storage

  val set : sint -> storage -> unit * storage
  val get : unit -> storage -> sint * storage
  val lt : sint * sint -> storage -> bool * storage
  val gt : sint * sint -> storage -> bool * storage
  val lte : sint * sint -> storage -> bool * storage
  val gte : sint * sint -> storage -> bool * storage
  val xor : bool * bool -> storage -> bool * storage
  val xor2 : bool * bool -> storage -> bool * storage
  val max : sint * sint -> storage -> sint * storage
end = struct
  type storage = sint

  let set n _ = ((), n)
  let get () s = (s, s)
  let lt (x, y) s = (x < y, s)
  let gt (x, y) s = (x > y, s)
  let lte (x, y) s = (x <= y, s)
  let gte (x, y) s = (x >= y, s)

  let xor (x, y) s =
    let b = (x || y) && not (x && y) in
    (b, s)

  let xor2 (x, y) s =
    let b = if x then (if y then false else true) else (if y then true else false) in
    (b, s)

  let max (n, m) s =
    let b = n > m in
    ((if b then n else m), s)
end
