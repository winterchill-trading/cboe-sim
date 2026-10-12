(** Cboe US Equities BOE v2 (Binary Order Entry): framing, session and in-scope
    application messages.

    Every message starts with a 10-byte header: [0xBA 0xBA], MessageLength u16 (bytes
    after the two start bytes), MessageType u8, MatchingUnit u8, SequenceNumber u32.
    Integers are little endian, prices have 4 implied decimals ({!Price4}), text is NUL
    padded, times are ns since the epoch. *)

open! Core

module Message_type : sig
  val login_request : int
  val logout_request : int
  val client_heartbeat : int
  val new_order : int
  val cancel_order : int
  val modify_order : int
  val login_response : int
  val logout : int
  val server_heartbeat : int
  val replay_complete : int
  val order_acknowledgment : int
  val order_rejected : int
  val order_modified : int
  val order_restated : int
  val user_modify_rejected : int
  val order_cancelled : int
  val cancel_rejected : int
  val order_execution : int

  (** Outbound application messages that take return bitfields. *)
  val with_return_bitfields : int list

  val name : int -> string
end

val header_length : int

(** Total length of the frame starting at [pos], once its first 4 bytes are available.
    Errors when the start bytes are not [0xBA 0xBA]. *)
val frame_length : Bigstring.t -> pos:int -> int Or_error.t

module Unit_sequences : sig
  type t =
    { no_unspecified_unit_replay : bool
    ; units : (int * int) list (** matching unit, last sequence seen *)
    }
  [@@deriving sexp, compare, equal]
end

module Login_request : sig
  type t =
    { session_sub_id : string
    ; username : string
    ; password : string
    ; unit_sequences : Unit_sequences.t option
    ; return_bitfields : (int * string) list
    (** outbound message type, bitfield bytes chosen for it *)
    }
  [@@deriving sexp, compare, equal]
end

module Login_response : sig
  type t =
    { status : char (** [A] accepted, [N] not authorised, [B] in use, [F] bitfields ... *)
    ; text : string
    ; no_unspecified_unit_replay : bool
    ; last_received_sequence : int
    ; units : (int * int) list
    ; return_bitfields : (int * string) list (** echoed *)
    }
  [@@deriving sexp, compare, equal]
end

module Logout : sig
  type t =
    { reason : char (** [U] user requested, [E] end of day, [A] admin, [!] violation *)
    ; text : string
    ; last_received_sequence : int
    ; units : (int * int) list
    }
  [@@deriving sexp, compare, equal]
end

module New_order : sig
  (** Fixed part plus the optional fields a simulator understands. Other optional fields
      are accepted on decode and dropped. *)
  type t =
    { cl_ord_id : string
    ; side : char (** [1] buy, [2] sell, [5] sell short, [6] short exempt *)
    ; order_qty : int
    ; clearing_firm : string option
    ; price : Price4.t option
    ; exec_inst : char option
    ; ord_type : char option
    ; time_in_force : char option
    ; min_qty : int option
    ; max_floor : int option
    ; symbol : string option (** required by Cboe; validated by the handler *)
    ; capacity : char option (** required by Cboe *)
    ; routing_inst : string option
    ; account : string option
    ; display_indicator : char option
    ; prevent_match : string option
    ; maturity_date : int option (** options: [YYYYMMDD] *)
    ; strike_price : Price4.t option
    ; put_or_call : char option (** [0] put, [1] call *)
    ; open_close : char option
    }
  [@@deriving sexp, compare, equal]

  (** Only [cl_ord_id], [side], [order_qty], [symbol] and [capacity] set. *)
  val create
    :  cl_ord_id:string
    -> side:char
    -> order_qty:int
    -> symbol:string
    -> ?capacity:char
    -> unit
    -> t
end

module Cancel_order : sig
  type t = { orig_cl_ord_id : string } [@@deriving sexp, compare, equal]
end

module Modify_order : sig
  type t =
    { cl_ord_id : string
    ; orig_cl_ord_id : string
    ; order_qty : int option
    ; price : Price4.t option
    ; ord_type : char option
    ; cancel_orig_on_reject : char option
    ; side : char option
    ; max_floor : int option
    }
  [@@deriving sexp, compare, equal]
end

module Return_fields : sig
  (** Values a server may return in an outbound message's optional fields. A field the
      session selected but the server has no value for is sent as zero bytes. *)
  type t =
    { side : char option
    ; price : Price4.t option
    ; exec_inst : char option
    ; ord_type : char option
    ; time_in_force : char option
    ; min_qty : int option
    ; symbol : string option
    ; capacity : char option
    ; account : string option
    ; clearing_firm : string option
    ; display_indicator : char option
    ; max_floor : int option
    ; order_qty : int option
    ; prevent_match : string option
    ; maturity_date : int option
    ; strike_price : Price4.t option
    ; put_or_call : char option
    ; open_close : char option
    ; orig_cl_ord_id : string option
    ; leaves_qty : int option
    ; last_shares : int option
    ; last_px : Price4.t option
    ; display_price : Price4.t option
    ; working_price : Price4.t option
    ; base_liquidity_indicator : char option
    }
  [@@deriving sexp, compare, equal]

  val empty : t
end

module Inbound : sig
  type t =
    | Login_request of Login_request.t
    | Logout_request
    | Client_heartbeat
    | New_order of New_order.t
    | Cancel_order of Cancel_order.t
    | Modify_order of Modify_order.t
  [@@deriving sexp, compare, equal]

  (** Session messages carry sequence 0; application messages the member's sequence. *)
  val to_string : sequence:int -> t -> string

  (** Decodes the frame at [pos] of length [len]: [(sequence, message)]. *)
  val of_bigstring : Bigstring.t -> pos:int -> len:int -> (int * t) Or_error.t

  val of_string : string -> (int * t) Or_error.t
end

module Outbound : sig
  type t =
    | Login_response of Login_response.t
    | Logout of Logout.t
    | Server_heartbeat
    | Replay_complete
    | Order_acknowledgment of
        { transaction_time : int
        ; cl_ord_id : string
        ; order_id : int
        ; return : Return_fields.t
        }
    | Order_rejected of
        { transaction_time : int
        ; cl_ord_id : string
        ; reason : char
        ; text : string
        ; return : Return_fields.t
        }
    | Order_modified of
        { transaction_time : int
        ; cl_ord_id : string
        ; order_id : int
        ; return : Return_fields.t
        }
    | Order_restated of
        { transaction_time : int
        ; cl_ord_id : string
        ; order_id : int
        ; reason : char (** [R] reserve reload *)
        ; return : Return_fields.t
        }
    | User_modify_rejected of
        { transaction_time : int
        ; cl_ord_id : string
        ; reason : char
        ; text : string
        ; return : Return_fields.t
        }
    | Order_cancelled of
        { transaction_time : int
        ; cl_ord_id : string
        ; reason : char
        ; return : Return_fields.t
        }
    | Cancel_rejected of
        { transaction_time : int
        ; cl_ord_id : string
        ; reason : char
        ; text : string
        ; return : Return_fields.t
        }
    | Order_execution of
        { transaction_time : int
        ; cl_ord_id : string
        ; exec_id : int
        ; last_shares : int
        ; last_px : Price4.t
        ; leaves_qty : int
        ; base_liquidity_indicator : char (** [A] added, [R] removed, [C] auction *)
        ; sub_liquidity_indicator : char
        ; contra_broker : string
        ; return : Return_fields.t
        }
  [@@deriving sexp, compare, equal]

  val message_type : t -> int

  (** Whether the message carries a sequence number and is replayed after a reconnect:
      acknowledgements, modifications, restatements, cancels and executions. *)
  val is_sequenced : t -> bool

  (** [return_bitfields msg_type] are the bitfield bytes the session chose at login for
      that message type ([""] for none). *)
  val to_string
    :  matching_unit:int
    -> sequence:int
    -> return_bitfields:(int -> string)
    -> t
    -> string

  (** [(matching_unit, sequence, message)]. *)
  val of_bigstring : Bigstring.t -> pos:int -> len:int -> (int * int * t) Or_error.t

  val of_string : string -> (int * int * t) Or_error.t
end

(** Execution ids print as 9 base-36 characters. *)
val exec_id_to_string : int -> string
