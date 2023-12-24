open OCamYul.Primitives

module ERC20 : sig
  type storage
  type mut_storage

  val total_supply : unit -> storage -> int * storage
  val balance_of : address -> storage -> mut_storage -> int * storage
  val mint : int -> storage -> mut_storage -> unit * storage
  val burn : int -> storage -> mut_storage -> unit * storage
  val transfer : address * int -> storage -> mut_storage -> unit * storage
end = struct
  type storage = int
  type mut_storage = (address, int) Hashtbl.t

  let total_supply () total = (total, total)
  let balance_of account total h = (Hashtbl.find h account, total)

  let mint n total h =
    let from_address = caller () in
    let from_balance = Hashtbl.find h from_address in
    Hashtbl.add h from_address (from_balance + n);
    ((), total + n)

  let burn n total h =
    let from_address = caller () in
    let from_balance = Hashtbl.find h from_address in
    Hashtbl.add h from_address (from_balance - n);
    ((), total - n)

  let transfer (to_address, n) total h =
    let from_address = caller () in
    let from_balance = Hashtbl.find h from_address in
    let to_balance = Hashtbl.find h to_address in
    Hashtbl.add h from_address (from_balance - n);
    Hashtbl.add h to_address (to_balance + n);
    ((), total)
end
