(** Reading BOE and PITCH frames from an Async [Reader.t], for the server and clients. *)

open! Core
open! Async

(** Calls [f] with each complete BOE frame until end of file, a malformed header, or [f]
    returning false. *)
val read_boe_frames : Reader.t -> f:(string -> bool) -> unit Deferred.t

(** Calls [f] with each complete PITCH Sequenced Unit Header frame until end of file or
    [f] returning false. *)
val read_pitch_frames : Reader.t -> f:(string -> bool) -> unit Deferred.t
