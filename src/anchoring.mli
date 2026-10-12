(** Anchoring a listing's noise traders to a reference price.

    Cboe has no free real-time source, so there is no reference depth or trade flow to
    shadow or replay (see the price-anchor design, "TradFi simulators"). Instead the noise
    traders' random walk ({!Exchange_core_engine.Market_sim}) is pulled towards a
    reference price with a half-life ([Market_sim.set_reference ~half_life]). The
    reference is a {!Target.t}: a series of [(time, price)] points, a scaled recorded
    crypto series, or a fixed start price with a drift.

    The anchor follows only while the venue says so ({!update}'s [follow], in practice the
    core session outside auctions and LULD pauses). When it stops following it hands the
    symbol back to the free walk ([Market_sim.clear_reference]); the venue's auctions and
    pauses own the price, and the anchor waits. Every effect goes through [Market_sim],
    whose calls are journaled, so the anchor reads no clock of its own. *)

open! Core
open! Async
open! Exchange_core

module Target : sig
  type t =
    | Series of (Time_ns.t * Price.t) array
    (** non-empty, in time order; the first point is mapped to the anchor's start *)
    | Fixed of
        { price : Price.t
        ; drift_bps_per_day : float (** compounded, per day of venue time *)
        }
  [@@deriving sexp_of]

  (** Sorts the points by time; fails when empty or a price is not positive. *)
  val series : (Time_ns.t * Price.t) list -> t Or_error.t

  (** Fails when the price is not positive. *)
  val fixed : ?drift_bps_per_day:float -> Price.t -> t Or_error.t

  (** A series file in price-anchor's format ([<time> <price>] per line, see
      [Price_anchor.Source.parse_series_line]), prices multiplied by [factor] (default
      [1.]). *)
  val load_series : ?factor:float -> string -> t Or_error.t Deferred.t

  (** A recorded reference from normalized event logs ([Price_anchor.Source.of_norm]):
      trade prices and BBO mids of [instrument] for the UTC days [[from, to_]], at most
      one point per [resolution] (default 1 s, the last price in each interval),
      multiplied by [factor]. L2 and funding events are ignored. *)
  val load_norm
    :  ?resolution:Time_ns.Span.t
    -> factor:float
    -> dir:string
    -> Market_data.Instrument.t
    -> from:Date.t
    -> to_:Date.t
    -> t Or_error.t Deferred.t

  (** The reference price [elapsed] after the anchor started. A series is a step function
      of its points, offset so its first point is at [elapsed = 0]; it holds its last
      price after the end. *)
  val price_at : t -> elapsed:Time_ns.Span.t -> Price.t

  (** [<price>[,<drift_bps_per_day>]]. *)
  val parse_fixed : string -> t Or_error.t
end

module State : sig
  type t =
    | Waiting (** the venue owns the price; the walk is free *)
    | Following (** the walk is pulled towards the target *)
  [@@deriving sexp, compare, equal]
end

type t

(** [half_life] defaults to 1 s. *)
val create : ?half_life:Time_ns.Span.t -> Target.t -> t

val target : t -> Target.t
val state : t -> State.t

(** The last price passed to [Market_sim.set_reference], while following. *)
val last_set : t -> Price.t option

(** Transitions to {!State.Following} and to {!State.Waiting}. *)
val resumes : t -> int

val waits : t -> int

(** Called on every venue tick before the noise traders' tick. The first call fixes the
    start ([elapsed = 0]). With [follow], pins [symbol]'s walk to the target price at
    [now] (when it changed or on resuming); without, hands the walk back once. *)
val update
  :  t
  -> now:Time_ns.t
  -> follow:bool
  -> sim:Exchange_core_engine.Market_sim.t
  -> symbol:Symbol.t
  -> unit
