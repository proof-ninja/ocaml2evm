open OCamYul.Primitives

module ERC20 : sig
  type storage
  type mut_storage

  val total_supply : unit -> storage -> int * storage
  val balance_of : address -> storage -> mut_storage -> int * storage
  val allowance : address * address -> storage -> mut_storage -> int * storage
  val mint : int -> storage -> mut_storage -> unit * storage
  val burn : int -> storage -> mut_storage -> unit * storage
  val transfer : address * int -> storage -> mut_storage -> unit * storage
  val approve : address * int -> storage -> mut_storage -> unit * storage

  val transfer_from :
    address * address * int -> storage -> mut_storage -> unit * storage
end = struct
  type storage = int

  type mut_storage =
    (address, int) Hashtbl.t * (address, (address, int) Hashtbl.t) Hashtbl.t

  let total_supply () total = (total, total)

  let balance_of account total (balance, _) =
    (Hashtbl.find balance account, total)

  let allowance (owner, allowed_address) total (_, allow) =
    (Hashtbl.find (Hashtbl.find allow owner) allowed_address, total)

  let mint amount total (balance, _) =
    let from_address = caller () in
    let from_balance = Hashtbl.find balance from_address in
    Hashtbl.replace balance from_address (from_balance + amount);
    ((), total + amount)

  let burn amount total (balance, _) =
    let from_address = caller () in
    let from_balance = Hashtbl.find balance from_address in
    Hashtbl.replace balance from_address (from_balance - amount);
    ((), total - amount)

  let transfer (to_address, amount) total (balance, _) =
    let from_address = caller () in
    let from_balance = Hashtbl.find balance from_address in
    let to_balance = Hashtbl.find balance to_address in
    Hashtbl.replace balance from_address (from_balance - amount);
    Hashtbl.replace balance to_address (to_balance + amount);
    ((), total)

  let approve (allowed_address, amount) total (_, allow) =
    Hashtbl.replace (Hashtbl.find allow (caller ())) allowed_address amount;
    ((), total)

  let transfer_from (from_address, to_address, amount) total (balance, allow) =
    let from_address_allow = Hashtbl.find allow from_address in
    let allowed_balance = Hashtbl.find from_address_allow (caller ()) in
    Hashtbl.replace from_address_allow (caller ()) (allowed_balance - amount);
    let from_balance = Hashtbl.find balance from_address in
    let to_balance = Hashtbl.find balance to_address in
    Hashtbl.replace balance from_address (from_balance - amount);
    Hashtbl.replace balance to_address (to_balance + amount);
    ((), total)
end
