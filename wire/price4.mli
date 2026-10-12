(** Cboe binary prices: signed integers with 4 implied decimals ([1234500] is $123.45).
    PITCH short prices have 2 implied decimals. *)

open! Core

type t = int [@@deriving sexp, compare, equal]

(** Parses ["123.45"]; more than 4 decimals raise. *)
val of_string : string -> t

(** ["123.45"], ["0.0001"], ["100"]: no trailing zeros. *)
val to_string : t -> string

(** Converts from 4 to 2 implied decimals; raises unless the price is a whole cent. *)
val to_short_exn : t -> int

val of_short : int -> t
