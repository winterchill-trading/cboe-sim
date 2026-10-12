(** The simulated listings: a handful of US equities on unit 1 and a few SPY option series
    (OSI symbology) on unit 2, with reference prices for the noise traders. *)

open! Core
open Equities_core
module Price := Exchange_core.Price

module Kind : sig
  type t =
    | Equity of { tier : Halts.Tier.t }
    | Option of
        { osi : Symbol.Osi.t
        ; option_class : Tick_size.Option_class.t
        }
  [@@deriving sexp_of]
end

type t =
  { symbol : string (** BOE and PITCH symbol: the ticker, or the 6-char feed symbol *)
  ; kind : Kind.t
  ; unit : int (** PITCH and BOE matching unit *)
  ; reference_price : Price.t (** start of the noise walk and prior close *)
  }
[@@deriving sexp_of]

val equities_unit : int
val options_unit : int

(** AAPL, MSFT, NVDA, AMZN, SPY and the test symbol ZVZZT. *)
val default_equities : t list

(** Calls and puts on SPY expiring 2026-12-18 at strikes 590, 600 and 610. *)
val spy_options : t list

(** [default_equities] restricted to [symbols] (all when [None]), plus [spy_options]
    unless [options] is false. Raises on an unknown symbol. *)
val universe : ?symbols:string list -> ?options:bool -> unit -> t list

val is_option : t -> bool

(** The engine instrument: whole shares or contracts, a $0.01 tick (every listing here
    trades above $1, and the SPY class is penny everywhere), BZX-like fees. *)
val to_engine : t -> Exchange_core.Instrument.t

val find : t list -> string -> t option

(** Finds an option by root symbol, [YYYYMMDD] maturity, strike and [0] put / [1] call, as
    BOE byte-4 fields express it. *)
val find_option
  :  t list
  -> root:string
  -> maturity:int
  -> strike:Cboe_wire.Price4.t
  -> put_or_call:char
  -> t option

(** Valid limit price for the listing: equity tick (Rule 612) or option class tick. *)
val is_valid_price : t -> Price.t -> bool

(** Conversions between engine prices (8 decimals) and wire prices (4). *)
val to_price4 : Price.t -> Cboe_wire.Price4.t

val of_price4 : Cboe_wire.Price4.t -> Price.t
