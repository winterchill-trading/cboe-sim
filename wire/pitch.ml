open! Core

let sym b ~pos ~len s = Le.set_text b ~pad:Space ~pos ~len s
let read_sym b ~pos ~len = Le.get_text b ~pad:Space ~pos ~len

module Message = struct
  type t =
    | Time of { seconds : int }
    | Add_order of
        { time_offset : int
        ; order_id : int
        ; side : char
        ; quantity : int
        ; symbol : string
        ; price : Price4.t
        ; flags : int
        }
    | Order_executed of
        { time_offset : int
        ; order_id : int
        ; executed_quantity : int
        ; execution_id : int
        ; trade_condition : char option
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
        ; flags : int
        }
    | Delete_order of
        { time_offset : int
        ; order_id : int
        }
    | Trade of
        { time_offset : int
        ; order_id : int
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
        ; status : char
        ; reg_sho_action : char
        }
    | Auction_update of
        { time_offset : int
        ; symbol : string
        ; auction_type : char
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
        ; symbol_condition : char
        ; underlying : string
        }
    | Spin_response of
        { sequence : int
        ; order_count : int
        ; status : char
        }
    | Spin_finished of { sequence : int }
    | Unknown of
        { message_type : int
        ; payload : string
        }
  [@@deriving sexp, compare, equal]

  let message_type = function
    | Time _ -> 0x20
    | Add_order _ -> 0x21
    | Order_executed _ -> 0x23
    | Order_executed_at_price _ -> 0x24
    | Reduce_size _ -> 0x25
    | Modify_order _ -> 0x27
    | Delete_order _ -> 0x29
    | Trade _ -> 0x2A
    | End_of_session _ -> 0x2D
    | Trading_status _ -> 0x31
    | Auction_update _ -> 0x95
    | Auction_summary _ -> 0x96
    | Symbol_mapping _ -> 0x2E
    | Spin_response _ -> 0x82
    | Spin_finished _ -> 0x83
    | Unknown { message_type; _ } -> message_type
  ;;

  let tc = function
    | None -> 0
    | Some _ -> 1
  ;;

  let length = function
    | Time _ -> 6
    | Add_order _ -> 34
    | Order_executed { trade_condition; _ } -> 26 + tc trade_condition
    | Order_executed_at_price { trade_condition; _ } -> 38 + tc trade_condition
    | Reduce_size _ -> 18
    | Modify_order _ -> 27
    | Delete_order _ -> 14
    | Trade { trade_condition; _ } -> 41 + tc trade_condition
    | End_of_session _ -> 6
    | Trading_status _ -> 18
    | Auction_update _ -> 47
    | Auction_summary _ -> 27
    | Symbol_mapping _ -> 38
    | Spin_response _ -> 11
    | Spin_finished _ -> 6
    | Unknown { payload; _ } -> 2 + String.length payload
  ;;

  let write b ~pos t =
    let len = length t in
    Bigstring.memset b ~pos ~len '\000';
    Le.set_u8 b ~pos len;
    Le.set_u8 b ~pos:(pos + 1) (message_type t);
    let at o = pos + o in
    let offset o = Le.set_u32 b ~pos:(at 2) o in
    let tc_at o c = Option.iter c ~f:(Le.set_char b ~pos:(at o)) in
    (match t with
     | Time { seconds } -> Le.set_u32 b ~pos:(at 2) seconds
     | Add_order { time_offset; order_id; side; quantity; symbol; price; flags } ->
       offset time_offset;
       Le.set_u64 b ~pos:(at 6) order_id;
       Le.set_char b ~pos:(at 14) side;
       Le.set_u32 b ~pos:(at 15) quantity;
       sym b ~pos:(at 19) ~len:6 symbol;
       Le.set_i64 b ~pos:(at 25) price;
       Le.set_u8 b ~pos:(at 33) flags
     | Order_executed
         { time_offset; order_id; executed_quantity; execution_id; trade_condition } ->
       offset time_offset;
       Le.set_u64 b ~pos:(at 6) order_id;
       Le.set_u32 b ~pos:(at 14) executed_quantity;
       Le.set_u64 b ~pos:(at 18) execution_id;
       tc_at 26 trade_condition
     | Order_executed_at_price
         { time_offset
         ; order_id
         ; executed_quantity
         ; remaining_quantity
         ; execution_id
         ; price
         ; trade_condition
         } ->
       offset time_offset;
       Le.set_u64 b ~pos:(at 6) order_id;
       Le.set_u32 b ~pos:(at 14) executed_quantity;
       Le.set_u32 b ~pos:(at 18) remaining_quantity;
       Le.set_u64 b ~pos:(at 22) execution_id;
       Le.set_i64 b ~pos:(at 30) price;
       tc_at 38 trade_condition
     | Reduce_size { time_offset; order_id; canceled_quantity } ->
       offset time_offset;
       Le.set_u64 b ~pos:(at 6) order_id;
       Le.set_u32 b ~pos:(at 14) canceled_quantity
     | Modify_order { time_offset; order_id; quantity; price; flags } ->
       offset time_offset;
       Le.set_u64 b ~pos:(at 6) order_id;
       Le.set_u32 b ~pos:(at 14) quantity;
       Le.set_i64 b ~pos:(at 18) price;
       Le.set_u8 b ~pos:(at 26) flags
     | Delete_order { time_offset; order_id } ->
       offset time_offset;
       Le.set_u64 b ~pos:(at 6) order_id
     | Trade
         { time_offset
         ; order_id
         ; side
         ; quantity
         ; symbol
         ; price
         ; execution_id
         ; trade_condition
         } ->
       offset time_offset;
       Le.set_u64 b ~pos:(at 6) order_id;
       Le.set_char b ~pos:(at 14) side;
       Le.set_u32 b ~pos:(at 15) quantity;
       sym b ~pos:(at 19) ~len:6 symbol;
       Le.set_i64 b ~pos:(at 25) price;
       Le.set_u64 b ~pos:(at 33) execution_id;
       tc_at 41 trade_condition
     | End_of_session { time_offset } -> offset time_offset
     | Trading_status { time_offset; symbol; status; reg_sho_action } ->
       offset time_offset;
       sym b ~pos:(at 6) ~len:8 symbol;
       Le.set_char b ~pos:(at 14) status;
       Le.set_char b ~pos:(at 15) reg_sho_action
     | Auction_update
         { time_offset
         ; symbol
         ; auction_type
         ; reference_price
         ; buy_shares
         ; sell_shares
         ; indicative_price
         ; auction_only_price
         } ->
       offset time_offset;
       sym b ~pos:(at 6) ~len:8 symbol;
       Le.set_char b ~pos:(at 14) auction_type;
       Le.set_i64 b ~pos:(at 15) reference_price;
       Le.set_u32 b ~pos:(at 23) buy_shares;
       Le.set_u32 b ~pos:(at 27) sell_shares;
       Le.set_i64 b ~pos:(at 31) indicative_price;
       Le.set_i64 b ~pos:(at 39) auction_only_price
     | Auction_summary { time_offset; symbol; auction_type; price; shares } ->
       offset time_offset;
       sym b ~pos:(at 6) ~len:8 symbol;
       Le.set_char b ~pos:(at 14) auction_type;
       Le.set_i64 b ~pos:(at 15) price;
       Le.set_u32 b ~pos:(at 23) shares
     | Symbol_mapping { feed_symbol; osi_symbol; symbol_condition; underlying } ->
       sym b ~pos:(at 2) ~len:6 feed_symbol;
       sym b ~pos:(at 8) ~len:21 osi_symbol;
       Le.set_char b ~pos:(at 29) symbol_condition;
       sym b ~pos:(at 30) ~len:8 underlying
     | Spin_response { sequence; order_count; status } ->
       Le.set_u32 b ~pos:(at 2) sequence;
       Le.set_u32 b ~pos:(at 6) order_count;
       Le.set_char b ~pos:(at 10) status
     | Spin_finished { sequence } -> Le.set_u32 b ~pos:(at 2) sequence
     | Unknown { payload; _ } ->
       String.iteri payload ~f:(fun i c -> Le.set_char b ~pos:(at (2 + i)) c));
    len
  ;;

  let to_string t =
    let b = Bigstring.create (length t) in
    ignore (write b ~pos:0 t : int);
    Bigstring.to_string b
  ;;

  let of_bigstring b ~pos ~limit =
    let open Or_error.Let_syntax in
    let%bind () =
      if pos + 2 <= limit then Ok () else Or_error.error_string "truncated message"
    in
    let len = Le.get_u8 b ~pos in
    let typ = Le.get_u8 b ~pos:(pos + 1) in
    let%bind () =
      if len >= 2 && pos + len <= limit
      then Ok ()
      else Or_error.error_s [%message "bad message length" (len : int) (typ : int)]
    in
    let at o = pos + o in
    let need n =
      if len >= n
      then Ok ()
      else Or_error.error_s [%message "message too short" (typ : int) (len : int)]
    in
    let u32 o = Le.get_u32 b ~pos:(at o) in
    let u64 o = Le.get_u64 b ~pos:(at o) in
    let i64 o = Le.get_i64 b ~pos:(at o) in
    let ch o = Le.get_char b ~pos:(at o) in
    let tc o = if len > o then Some (ch o) else None in
    let%map (msg : t) =
      match typ with
      | 0x20 ->
        let%map () = need 6 in
        Time { seconds = u32 2 }
      | 0x21 ->
        let%map () = need 34 in
        Add_order
          { time_offset = u32 2
          ; order_id = u64 6
          ; side = ch 14
          ; quantity = u32 15
          ; symbol = read_sym b ~pos:(at 19) ~len:6
          ; price = i64 25
          ; flags = Le.get_u8 b ~pos:(at 33)
          }
      | 0x23 ->
        let%map () = need 26 in
        Order_executed
          { time_offset = u32 2
          ; order_id = u64 6
          ; executed_quantity = u32 14
          ; execution_id = u64 18
          ; trade_condition = tc 26
          }
      | 0x24 ->
        let%map () = need 38 in
        Order_executed_at_price
          { time_offset = u32 2
          ; order_id = u64 6
          ; executed_quantity = u32 14
          ; remaining_quantity = u32 18
          ; execution_id = u64 22
          ; price = i64 30
          ; trade_condition = tc 38
          }
      | 0x25 ->
        let%map () = need 18 in
        Reduce_size { time_offset = u32 2; order_id = u64 6; canceled_quantity = u32 14 }
      | 0x27 ->
        let%map () = need 27 in
        Modify_order
          { time_offset = u32 2
          ; order_id = u64 6
          ; quantity = u32 14
          ; price = i64 18
          ; flags = Le.get_u8 b ~pos:(at 26)
          }
      | 0x29 ->
        let%map () = need 14 in
        Delete_order { time_offset = u32 2; order_id = u64 6 }
      | 0x2A ->
        let%map () = need 41 in
        Trade
          { time_offset = u32 2
          ; order_id = u64 6
          ; side = ch 14
          ; quantity = u32 15
          ; symbol = read_sym b ~pos:(at 19) ~len:6
          ; price = i64 25
          ; execution_id = u64 33
          ; trade_condition = tc 41
          }
      | 0x2D ->
        let%map () = need 6 in
        End_of_session { time_offset = u32 2 }
      | 0x31 ->
        let%map () = need 18 in
        Trading_status
          { time_offset = u32 2
          ; symbol = read_sym b ~pos:(at 6) ~len:8
          ; status = ch 14
          ; reg_sho_action = ch 15
          }
      | 0x95 ->
        let%map () = need 47 in
        Auction_update
          { time_offset = u32 2
          ; symbol = read_sym b ~pos:(at 6) ~len:8
          ; auction_type = ch 14
          ; reference_price = i64 15
          ; buy_shares = u32 23
          ; sell_shares = u32 27
          ; indicative_price = i64 31
          ; auction_only_price = i64 39
          }
      | 0x96 ->
        let%map () = need 27 in
        Auction_summary
          { time_offset = u32 2
          ; symbol = read_sym b ~pos:(at 6) ~len:8
          ; auction_type = ch 14
          ; price = i64 15
          ; shares = u32 23
          }
      | 0x2E ->
        let%map () = need 38 in
        Symbol_mapping
          { feed_symbol = read_sym b ~pos:(at 2) ~len:6
          ; osi_symbol = read_sym b ~pos:(at 8) ~len:21
          ; symbol_condition = ch 29
          ; underlying = read_sym b ~pos:(at 30) ~len:8
          }
      | 0x82 ->
        let%map () = need 11 in
        Spin_response { sequence = u32 2; order_count = u32 6; status = ch 10 }
      | 0x83 ->
        let%map () = need 6 in
        Spin_finished { sequence = u32 2 }
      | message_type ->
        Ok
          (Unknown
             { message_type
             ; payload = Bigstring.To_string.sub b ~pos:(at 2) ~len:(len - 2)
             })
    in
    msg, len
  ;;
end

module Frame = struct
  type t =
    { unit : int
    ; sequence : int
    ; messages : Message.t list
    }
  [@@deriving sexp, compare, equal]

  let header_length = 8

  let to_string t =
    let total = header_length + List.sum (module Int) t.messages ~f:Message.length in
    let b = Bigstring.create total in
    Le.set_u16 b ~pos:0 total;
    Le.set_u8 b ~pos:2 (List.length t.messages);
    Le.set_u8 b ~pos:3 t.unit;
    Le.set_u32 b ~pos:4 t.sequence;
    ignore
      (List.fold t.messages ~init:header_length ~f:(fun pos m ->
         pos + Message.write b ~pos m)
       : int);
    Bigstring.to_string b
  ;;

  let frame_length b ~pos = Le.get_u16 b ~pos

  let of_bigstring b ~pos ~len =
    let open Or_error.Let_syntax in
    let%bind () =
      if len >= header_length && frame_length b ~pos = len
      then Ok ()
      else Or_error.error_s [%message "bad sequenced unit header" (len : int)]
    in
    let count = Le.get_u8 b ~pos:(pos + 2) in
    let limit = pos + len in
    let rec loop i p acc =
      if i = count
      then Ok (List.rev acc)
      else (
        let%bind m, n = Message.of_bigstring b ~pos:p ~limit in
        loop (i + 1) (p + n) (m :: acc))
    in
    let%map messages = loop 0 (pos + header_length) [] in
    { unit = Le.get_u8 b ~pos:(pos + 3)
    ; sequence = Le.get_u32 b ~pos:(pos + 4)
    ; messages
    }
  ;;

  let of_string s = of_bigstring (Bigstring.of_string s) ~pos:0 ~len:(String.length s)
end
