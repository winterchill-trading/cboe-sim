(** TCP front ends: the BOE order entry gateway, the PITCH feed (spin then live frames),
    and an HTTP port with a JSON websocket at [/ws] carrying the same feed events. *)

open! Core
module Sim_clock := Clock
open! Async

module Config : sig
  type t =
    { gateway_port : int (** 0 picks a free port *)
    ; feed_port : int
    ; http_port : int
    ; instruments : Instruments.t list
    ; venue : Venue.Config.t
    ; clock : Sim_clock.t
    }
end

type t

val start : Config.t -> t Deferred.t
val gateway_port : t -> int
val feed_port : t -> int
val http_port : t -> int
val venue : t -> Venue.t
val feed : t -> Feed.t
val close : t -> unit Deferred.t
