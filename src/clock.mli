(** The session clock. [Real] follows New York time and the NYSE calendar; [Accelerated]
    runs a virtual New York clock from a chosen start at [speed] times real time, so a
    whole trading day (pre-market, opening auction, core, closing auction, post-market)
    can be played in minutes; [Always_open] stays in the core session, for play. [Manual]
    is driven by tests. *)

open! Core
open Equities_core

type t

val real : unit -> t
val accelerated : start:Time_ns.t -> speed:float -> t
val always_open : unit -> t
val manual : Time_ns.t -> t

(** Moves a manual clock; raises for other clocks. *)
val set : t -> Time_ns.t -> unit

(** Virtual time. *)
val now : t -> Time_ns.t

val phase : t -> Session.Us_equities.Phase.t

(** Whether auctions and phase transitions happen (false for [Always_open]). *)
val has_sessions : t -> bool

(** New York local date and time of day of [now]. *)
val local : t -> Date.t * Time_ns.Span.t

(** [ny_time date "09:29:30"]: that New York local time on [date]. *)
val ny_time : Date.t -> string -> Time_ns.t
