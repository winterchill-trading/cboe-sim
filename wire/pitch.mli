(** Cboe Multicast PITCH 2.X messages and the Sequenced Unit Header, as carried over a TCP
    stream (the framing Cboe uses for gap and spin servers).

    Integers are little endian, long prices have 4 implied decimals, alphanumeric fields
    are space padded. Every message starts with [Length u8] (including itself) and
    [MessageType u8]. The simulator sends the long variants. Messages of the options unit
    carry an extra trade condition byte on executions and trades. *)

open! Core

module Message : sig
  type t =
    | Time of { seconds : int (** since midnight Eastern *) }
    | Add_order of
        { time_offset : int
        ; order_id : int
        ; side : char (** [B] or [S] *)
        ; quantity : int
        ; symbol : string
        ; price : Price4.t
        ; flags : int (** bit 0: displayed *)
        }
    | Order_executed of
        { time_offset : int
        ; order_id : int
        ; executed_quantity : int
        ; execution_id : int
        ; trade_condition : char option (** options only *)
        }
    | Order_executed_at_price of
        { time_offset : int
        ; order_id : int
        ; executed_quantity : int
        ; remaining_quantity : int
        ; execution_id : int
        ; price : Price4.t
        ; trade_condition : char option
        }
    | Reduce_size of
        { time_offset : int
        ; order_id : int
        ; canceled_quantity : int
        }
    | Modify_order of
        { time_offset : int
        ; order_id : int
        ; quantity : int
        ; price : Price4.t
        ; flags : int (** bit 1: priority maintained *)
        }
    | Delete_order of
        { time_offset : int
        ; order_id : int
        }
    | Trade of
        { time_offset : int
        ; order_id : int (** obfuscated (0) for hidden orders *)
        ; side : char
        ; quantity : int
        ; symbol : string
        ; price : Price4.t
        ; execution_id : int
        ; trade_condition : char option
        }
    | End_of_session of { time_offset : int }
    | Trading_status of
        { time_offset : int
        ; symbol : string
        ; status : char (** [H] halted, [Q] quote only, [R] rotation, [T] trading *)
        ; reg_sho_action : char (** [0] no restriction, [1] Rule 201 in effect *)
        }
    | Auction_update of
        { time_offset : int
        ; symbol : string
        ; auction_type : char (** [O] open, [C] close, [H] halt *)
        ; reference_price : Price4.t
        ; buy_shares : int
        ; sell_shares : int
        ; indicative_price : Price4.t
        ; auction_only_price : Price4.t
        }
    | Auction_summary of
        { time_offset : int
        ; symbol : string
        ; auction_type : char
        ; price : Price4.t
        ; shares : int
        }
    | Symbol_mapping of
        { feed_symbol : string
        ; osi_symbol : string
        ; symbol_condition : char (** [N] normal, [C] closing only *)
        ; underlying : string
        }
    | Spin_response of
        { sequence : int
        ; order_count : int
        ; status : char (** [A] accepted *)
        }
    | Spin_finished of { sequence : int }
    | Unknown of
        { message_type : int
        ; payload : string
        }
  [@@deriving sexp, compare, equal]

  val message_type : t -> int
  val length : t -> int
  val to_string : t -> string

  (** Decodes the message at [pos]; [limit] is the end of the enclosing frame. Returns the
      message and its length. *)
  val of_bigstring : Bigstring.t -> pos:int -> limit:int -> (t * int) Or_error.t
end

module Frame : sig
  type t =
    { unit : int
    ; sequence : int (** of the first message; 0 for unsequenced (spin) frames *)
    ; messages : Message.t list (** empty: heartbeat *)
    }
  [@@deriving sexp, compare, equal]

  val header_length : int
  val to_string : t -> string

  (** Total frame length, from the first 2 bytes at [pos]. *)
  val frame_length : Bigstring.t -> pos:int -> int

  val of_bigstring : Bigstring.t -> pos:int -> len:int -> t Or_error.t
  val of_string : string -> t Or_error.t
end
