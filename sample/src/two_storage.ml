open OCamYul.Primitives

module TwoStorage : sig
  type storage

  val set : uint * uint -> storage -> unit * storage
  val set_fst : uint -> storage -> unit * storage
  val set_snd : uint -> storage -> unit * storage
  val get : unit -> storage -> (uint * uint) * storage
  val get_fst : unit -> storage -> uint * storage
  val get_snd : unit -> storage -> uint * storage
  val swap : unit -> storage -> unit * storage
  val total : unit -> storage -> uint * storage
  val set_fst_snd : uint * uint -> storage -> unit * storage
  val get_fst_snd : unit -> storage -> (uint * uint) * storage
end = struct
  type storage = uint * uint

  let set (x, y) (_, _) = ((), (x, y))
  let set_fst x (_, s2) = ((), (x, s2))
  let set_snd x (s1, _) = ((), (s1, x))
  let get () (s1, s2) = ((s1, s2), (s1, s2))
  let get_fst () (s1, s2) = (s1, (s1, s2))
  let get_snd () (s1, s2) = (s2, (s1, s2))
  let swap () (s1, s2) = ((), (s2, s1))

  let total () (s1, s2) =
    let n = s1 +^ s2 in
    (n, (s1, s2))

  let set_fst_snd (x, y) s =
    let (), s = set_fst x s in
    let (), s = set_snd y s in
    ((), s)

  let get_fst_snd () s =
    let x, s = get_fst () s in
    let y, s = get_snd () s in
    ((x, y), s)
end
