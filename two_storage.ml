module TwoStorage : sig
  type storage

  val set : int * int -> storage -> unit * storage
  val get : unit -> storage -> (int * int) * storage
  val swap : unit -> storage -> unit * storage
  val total : unit -> storage -> int * storage
end = struct
  type storage = int * int

  let set (x, y) _ = ((), (x, y))
  let get _ (s1, s2) = ((s1, s2), (s1, s2))
  let swap _ (s1, s2) = ((), (s2, s1))

  let total _ (s1, s2) =
    let n = s1 + s2 in
    (n, (s1, s2))
end
