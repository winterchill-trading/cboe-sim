(** Anchoring a listing's noise traders to a reference price, over {!Price_anchor.Anchor}.

    Cboe has no free real-time source, so there is no reference depth or trade flow to
    shadow or replay (see the price-anchor design, "TradFi simulators"). Instead the noise
    traders' random walk ({!Exchange_core_engine.Market_sim}) is pulled towards a
    reference price with a half-life. The reference is a {!Target.t}: a
    {!Price_anchor.Path.t} (a series of [(time, price)] points, a recorded crypto series,
    or a fixed price with a drift) and the factor that scales it to the listing.

    The anchor follows only while the venue says so ([may_engage], in practice the core
    session outside auctions and LULD pauses). When it stops following it hands the symbol
    back to the free walk; the venue's auctions and pauses own the price. When it follows
    again the walk restarts from the venue's last print ([last_trade]). Targets are kept
    inside the LULD bands ([clamp]). Every effect goes through [Market_sim], whose calls
    are journaled, and the anchor reads the exchange's clock only. *)

open! Core
open! Async
open! Exchange_core

module Target : sig
  type t = private
    { path : Price_anchor.Path.t
    ; price_factor : float (** applied by the anchor, rounding to the tick *)
    ; reference : Market_data.Instrument.t option
    (** the recorded instrument, for [-anchor-scaled] *)
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

  (** A recorded reference from normalized event logs ([Price_anchor.Source.of_norm] then
      [Source.to_path]): the reference book's mid (L2 or BBO), else the last trade, of
      [instrument] for the UTC days [[from, to_]], keeping a point when the price changed
      and at least [resolution] (default 1 s) passed, multiplied by [factor]. *)
  val load_norm
    :  ?resolution:Time_ns.Span.t
    -> factor:float
    -> dir:string
    -> Market_data.Instrument.t
    -> from:Date.t
    -> to_:Date.t
    -> t Or_error.t Deferred.t

  (** The scaled reference price [elapsed] after the anchor started, before the anchor
      rounds it to the tick. A series holds its last price after the end. *)
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

(** One anchor for [symbol], started at the exchange's current time. [half_life] defaults
    to 1 s; [noise_traders] is the listing's count (kept trading the symbol). [reference]
    defaults to the target's, else [cboe:<symbol>]; it names the anchor's (idle) accounts. *)
val create
  :  ?half_life:Time_ns.Span.t
  -> noise_traders:int
  -> may_engage:(unit -> bool)
  -> last_trade:(unit -> Price.t option)
  -> clamp:(Price.t -> Price.t)
  -> market_sim:Exchange_core_engine.Market_sim.t
  -> exchange:Exchange_core_engine.Exchange.t
  -> symbol:Symbol.t
  -> Target.t
  -> t

val target : t -> Target.t

(** [Following] once a target was set and [may_engage] has not said no since. *)
val state : t -> State.t

(** The last target set, after the clamp; [None] until the first and from a pause until
    the next. *)
val last_set : t -> Price.t option

(** Times the anchor followed again after a pause (the first follow is not a resume). *)
val resumes : t -> int

(** Pauses: times the venue first said no, handing the walk back. *)
val waits : t -> int

val stats : t -> Price_anchor.Anchor.Stats.t

(** Called on every venue tick, after the exchange's clock was stepped and before the
    noise traders' tick. *)
val step : t -> unit
