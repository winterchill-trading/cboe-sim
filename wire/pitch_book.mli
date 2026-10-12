(** An order-by-order book per symbol rebuilt from PITCH messages (spin Add Orders, then
    the live stream), as a feed client keeps it. The simulator uses the same code to serve
    spins. *)

open! Core

module Order : sig
  type t =
    { order_id : int
    ; symbol : string
    ; side : char
    ; quantity : int
    ; price : Price4.t
    }
  [@@deriving sexp, compare, equal]
end

type t

val create : unit -> t

(** Applies Add Order, Order Executed (at price), Reduce Size, Modify Order and Delete
    Order; other messages are ignored. Messages about unknown orders are ignored too. *)
val apply : t -> unit:int -> Pitch.Message.t -> unit

(** Resting orders of a unit in arrival order (the order a spin replays them). *)
val orders : t -> unit:int -> Order.t list

val find : t -> unit:int -> int -> Order.t option

(** Aggregated levels, best first: [(price, total quantity, order count)]. *)
val levels : t -> symbol:string -> bids:bool -> (Price4.t * int * int) list
