(** Little-endian field access on [Bigstring.t] at absolute offsets, plus the two ASCII
    padding conventions: BOE pads text with NUL, PITCH with spaces. *)

open! Core

val get_u8 : Bigstring.t -> pos:int -> int
val set_u8 : Bigstring.t -> pos:int -> int -> unit
val get_u16 : Bigstring.t -> pos:int -> int
val set_u16 : Bigstring.t -> pos:int -> int -> unit
val get_u32 : Bigstring.t -> pos:int -> int
val set_u32 : Bigstring.t -> pos:int -> int -> unit

(** Signed 8 bytes; used for prices and timestamps. *)
val get_i64 : Bigstring.t -> pos:int -> int

val set_i64 : Bigstring.t -> pos:int -> int -> unit

(** Unsigned 8 bytes; values above [max_int] raise. *)
val get_u64 : Bigstring.t -> pos:int -> int

val set_u64 : Bigstring.t -> pos:int -> int -> unit
val get_char : Bigstring.t -> pos:int -> char

(** A NUL byte reads as ['\000']. *)
val set_char : Bigstring.t -> pos:int -> char -> unit

module Pad : sig
  type t =
    | Nul
    | Space
end

(** Left-justified text of exactly [len] bytes; longer strings are truncated. *)
val set_text : Bigstring.t -> pad:Pad.t -> pos:int -> len:int -> string -> unit

(** The text with trailing padding (NUL or spaces, whatever [pad]) removed. *)
val get_text : Bigstring.t -> pad:Pad.t -> pos:int -> len:int -> string

(** Hex dump with a space between bytes, for tests. *)
val hex : string -> string
