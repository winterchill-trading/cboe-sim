(** The PITCH feed: sequences venue events per matching unit, inserts Time messages
    (seconds since midnight Eastern, with nanosecond offsets in every message), keeps the
    displayed book for spins, and fans frames out to TCP subscribers (one message per
    Sequenced Unit Header frame) and to JSON subscribers.

    A new subscriber first gets, per unit and unsequenced: a Time message, Symbol Mapping
    for each option, Trading Status for each listing, then a spin (Spin Response, an Add
    Order per resting displayed order, Spin Finished). Live frames follow from the next
    sequence. *)

open! Core
open Cboe_wire

type t

val create : clock:Clock.t -> Instruments.t list -> t

(** Venue output. *)
val publish : t -> unit:int -> Pitch.Message.t -> unit

(** Sends Time messages when the second changed and heartbeats (count 0, next sequence) on
    units silent for a second. Call about every 100 ms. *)
val on_timer : t -> unit

(** Next sequence number of a unit. *)
val next_sequence : t -> unit:int -> int

type subscription

(** [send] receives binary frames; the snapshot is sent before this returns.
    [trading_status] gives the current status message of a listing. *)
val subscribe
  :  t
  -> trading_status:(Instruments.t -> Pitch.Message.t)
  -> send:(string -> unit)
  -> subscription

(** Same events as JSON objects, one per message, with [unit] and [seq] fields. *)
val subscribe_json
  :  t
  -> trading_status:(Instruments.t -> Pitch.Message.t)
  -> send:(string -> unit)
  -> subscription

val unsubscribe : t -> subscription -> unit
val subscriber_count : t -> int

(** JSON rendering of a message ([sexp]-free, prices as decimal strings). *)
val to_json : unit:int -> seq:int -> Pitch.Message.t -> Yojson.Safe.t

val book : t -> Pitch_book.t
