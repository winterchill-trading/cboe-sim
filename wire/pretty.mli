(** One-line human-readable renderings of BOE and PITCH messages, for the CLI and tests. *)

open! Core

val boe_outbound : Boe.Outbound.t -> string
val boe_inbound : Boe.Inbound.t -> string
val pitch : Pitch.Message.t -> string
