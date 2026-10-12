(** The simulated Cboe matching units: BOE order semantics on top of the venue-neutral
    engine, and the PITCH events those orders and the noise traders produce.

    What the engine runs directly: visible limit orders (Day and GTC rest as GTC), IOC,
    market orders, post-only (RoutingInst [P]). What this module adds on top:
    - FOK: checked against the opposite book before an IOC is sent;
    - hidden orders (DisplayIndicator [I]): rest in the engine but never shown on PITCH;
      fills against them print as Trade messages with an obfuscated order id;
    - reserve orders (MaxFloor): the engine holds one displayed slice; when it fills the
      order is reloaded with a new order id (BOE Order Restated, PITCH Add Order);
    - on-open and on-close orders (TimeInForce [2] and [7]): held here until the opening
      or closing auction, which uncrosses them with the book (equities-core [Auction]);
    - self-trade prevention [O] and [B] (the engine itself always cancels the newest);
    - session phases, LULD price bands and pauses, Rule 201 short sale checks;
    - modifies keep the BOE and PITCH order id although the engine re-queues the order.

    Every user message is reported through [emit_user] and every feed message through
    [emit_feed], in the order a member or a feed client would see them. *)

open! Core
open Cboe_wire

module Config : sig
  type t =
    { noise : bool
    (** run noise traders on every listing except test symbols such as ZVZZT *)
    ; seed : int
    ; noise_traders : int (** per listing *)
    ; auction_lead : Time_ns.Span.t
    (** Auction Update messages start this long before the open and the close *)
    ; anchors : (string * Anchoring.Target.t) list
    (** listings whose noise traders follow a reference price ({!Anchoring}); each needs
        noise traders, so not the test symbols *)
    ; anchor_half_life : Time_ns.Span.t
    (** how fast the noise walk is pulled towards the reference (default 1 s) *)
    }

  val default : t
end

type t

val create
  :  ?config:Config.t
  -> clock:Clock.t
  -> emit_user:(user:string -> unit:int -> Boe.Outbound.t -> unit)
  -> emit_feed:(unit:int -> Pitch.Message.t -> unit)
  -> Instruments.t list
  -> t

val instruments : t -> Instruments.t list
val clock : t -> Clock.t
val exchange : t -> Exchange_core_engine.Exchange.t

(** BOE application messages from the session [user] (username and session sub id). *)
val new_order : t -> user:string -> Boe.New_order.t -> unit

val cancel_order : t -> user:string -> Boe.Cancel_order.t -> unit
val modify_order : t -> user:string -> Boe.Modify_order.t -> unit

(** Advances the engine to the clock, runs phase transitions and auctions, LULD, Auction
    Updates and one noise trader round. Call every 100 ms. *)
val tick : t -> unit

(** The Trading Status message a feed client should see for a listing now. *)
val trading_status : t -> Instruments.t -> Pitch.Message.t

(** The listing's anchor, if [Config.anchors] names it. It follows the reference only in
    the core session outside the auction windows ([Config.auction_lead] before the open
    and the close) and LULD pauses; otherwise the venue owns the price and the walk is
    free; following again, the walk restarts from the venue's last print. Targets are kept
    inside the LULD bands. With the [Always_open] clock it always follows unless paused. *)
val anchor : t -> string -> Anchoring.t option

(** The LULD reference price: the mean of the listing's prints on this venue over the last
    five minutes, else the mark. Anchoring moves it only through those prints. *)
val luld_reference : t -> string -> Exchange_core.Price.t option

(** The listing's noise traders, when [Config.noise] is on. *)
val market_sim : t -> string -> Exchange_core_engine.Market_sim.t option

(** Resting order count of a user, for tests and logs. *)
val open_order_count : t -> user:string -> int
