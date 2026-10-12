(** BOE sessions: the per-member outbound message store (sequence numbers per matching
    unit, kept across connections for replay) and the per-connection state machine.

    A session is identified by username plus session sub id; any username and password is
    accepted. Only one connection per session at a time (status [B] otherwise). *)

open! Core
open Cboe_wire

module Store : sig
  type t

  val create : unit -> t

  (** Session key of a login, the [user] that {!Venue} sees. *)
  val key : username:string -> session_sub_id:string -> string

  (** Venue output: sequenced messages get the next sequence of their unit and are kept
      for replay; all messages go to the live connection, if any. *)
  val deliver : t -> user:string -> unit:int -> Boe.Outbound.t -> unit

  (** Last outbound sequence per unit. *)
  val unit_sequences : t -> user:string -> (int * int) list
end

module Handler : sig
  type t

  (** [send] writes a frame to the socket; [close] closes it. [now] is wall time, for
      heartbeats: the server sends one after 1 s of silence and logs the member out after
      [heartbeat_timeout] (default 5 s) without any inbound message. *)
  val create
    :  ?heartbeat_timeout:Time_ns.Span.t
    -> store:Store.t
    -> venue:Venue.t
    -> now:(unit -> Time_ns.t)
    -> send:(string -> unit)
    -> close:(unit -> unit)
    -> unit
    -> t

  (** One complete BOE frame from the member. *)
  val on_frame : t -> string -> unit

  (** Call about every 100 ms. *)
  val on_timer : t -> unit

  val on_disconnect : t -> unit
  val is_closed : t -> bool
end
