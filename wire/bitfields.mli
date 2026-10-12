(** BOE optional fields: [NumberOfBitfields u8], that many bitfield bytes, then the fields
    whose bits are set, in bitfield order (byte 1 bit 0 first).

    Field tables map a (byte, bit) to a field name and length. Values are kept raw (the
    little-endian or ASCII bytes as on the wire); {!Raw} converts. *)

open! Core

module Spec : sig
  type t =
    { name : string
    ; byte : int (** 1-based, as in the specification tables *)
    ; bit : int (** bit value: 1, 2, 4, ..., 128 *)
    ; len : int
    }
  [@@deriving sexp_of]
end

type table = Spec.t list

(** New Order input bitfields (bytes 1 to 4 and the fields of 5 and 6 used here). *)
val new_order : table

(** Modify Order input bitfields, byte 1 and 2. *)
val modify_order : table

(** Cancel Order input bitfields, byte 1. *)
val cancel_order : table

(** Return bitfields shared by every outbound application message, bytes 1 to 5. *)
val return : table

(** Raw value helpers. *)
module Raw : sig
  val u8 : int -> string
  val u16 : int -> string
  val u32 : int -> string
  val i64 : int -> string
  val char : char -> string

  (** NUL padded to [len]. *)
  val text : len:int -> string -> string

  val to_int : string -> int
  val to_char : string -> char
  val to_text : string -> string
end

(** Bitfield bytes with the bits of the named fields set, trailing zero bytes dropped.
    Raises on a name not in the table. *)
val bitfields_of_names : table -> string list -> string

(** Names of the fields selected by bitfield bytes, in wire order; an error names the
    first set bit that the table does not know. *)
val names_of_bitfields : table -> string -> string list Or_error.t

(** Writes count, bitfield bytes and the selected fields, taking values from [lookup]
    (missing values are written as zero bytes). Returns the number of bytes written. *)
val write
  :  table
  -> Bigstring.t
  -> pos:int
  -> bitfields:string
  -> lookup:(string -> string option)
  -> int

(** Bytes {!write} would produce. *)
val size : table -> bitfields:string -> int

(** Reads count, bitfields and fields at [pos]: the bitfield bytes and [(name, raw)] in
    wire order. *)
val read
  :  table
  -> Bigstring.t
  -> pos:int
  -> len:int
  -> (string * (string * string) list) Or_error.t
