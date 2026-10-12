open! Core
open Cboe_wire

let dump s =
  String.to_list s
  |> List.chunks_of ~length:10
  |> List.iteri ~f:(fun i row ->
    printf "%3d: %s\n" (i * 10) (Le.hex (String.of_char_list row)))
;;

let roundtrip (m : Pitch.Message.t) =
  let s = Pitch.Message.to_string m in
  printf "type 0x%02X length %d\n" (Pitch.Message.message_type m) (String.length s);
  dump s;
  let back, n =
    Pitch.Message.of_bigstring (Bigstring.of_string s) ~pos:0 ~limit:(String.length s)
    |> Or_error.ok_exn
  in
  if n <> String.length s || not (Pitch.Message.equal back m)
  then print_s [%message "round trip mismatch" (back : Pitch.Message.t)]
;;

let px = Price4.of_string

let%expect_test "add order long: offsets from the specification" =
  (* TimeOffset @2, OrderId @6, Side @14, Quantity @15, Symbol(6) @19, Price @25, AddFlags
     @33; 34 bytes. *)
  roundtrip
    (Add_order
       { time_offset = 1000
       ; order_id = 0x0102030405
       ; side = 'B'
       ; quantity = 100
       ; symbol = "AAPL"
       ; price = px "189.5"
       ; flags = 1
       });
  [%expect
    {|
    type 0x21 length 34
      0: 22 21 e8 03 00 00 05 04 03 02
     10: 01 00 00 00 42 64 00 00 00 41
     20: 41 50 4c 20 20 58 ea 1c 00 00
     30: 00 00 00 01
    |}]
;;

let%expect_test "executions, reductions, modifies and deletes" =
  roundtrip
    (Order_executed
       { time_offset = 1
       ; order_id = 5
       ; executed_quantity = 40
       ; execution_id = 9
       ; trade_condition = None
       });
  roundtrip
    (Order_executed
       { time_offset = 1
       ; order_id = 5
       ; executed_quantity = 4
       ; execution_id = 9
       ; trade_condition = Some ' '
       });
  roundtrip
    (Order_executed_at_price
       { time_offset = 2
       ; order_id = 5
       ; executed_quantity = 40
       ; remaining_quantity = 60
       ; execution_id = 10
       ; price = px "189.45"
       ; trade_condition = None
       });
  roundtrip (Reduce_size { time_offset = 3; order_id = 5; canceled_quantity = 10 });
  roundtrip
    (Modify_order
       { time_offset = 4; order_id = 5; quantity = 50; price = px "189.4"; flags = 0 });
  roundtrip (Delete_order { time_offset = 5; order_id = 5 });
  [%expect
    {|
    type 0x23 length 26
      0: 1a 23 01 00 00 00 05 00 00 00
     10: 00 00 00 00 28 00 00 00 09 00
     20: 00 00 00 00 00 00
    type 0x23 length 27
      0: 1b 23 01 00 00 00 05 00 00 00
     10: 00 00 00 00 04 00 00 00 09 00
     20: 00 00 00 00 00 00 20
    type 0x24 length 38
      0: 26 24 02 00 00 00 05 00 00 00
     10: 00 00 00 00 28 00 00 00 3c 00
     20: 00 00 0a 00 00 00 00 00 00 00
     30: 64 e8 1c 00 00 00 00 00
    type 0x25 length 18
      0: 12 25 03 00 00 00 05 00 00 00
     10: 00 00 00 00 0a 00 00 00
    type 0x27 length 27
      0: 1b 27 04 00 00 00 05 00 00 00
     10: 00 00 00 00 32 00 00 00 70 e6
     20: 1c 00 00 00 00 00 00
    type 0x29 length 14
      0: 0e 29 05 00 00 00 05 00 00 00
     10: 00 00 00 00
    |}]
;;

let%expect_test "trades, status, auctions and session" =
  roundtrip
    (Trade
       { time_offset = 6
       ; order_id = 0
       ; side = 'B'
       ; quantity = 200
       ; symbol = "MSFT"
       ; price = px "420.1"
       ; execution_id = 11
       ; trade_condition = None
       });
  roundtrip
    (Trading_status
       { time_offset = 7; symbol = "MSFT"; status = 'T'; reg_sho_action = '0' });
  roundtrip
    (Auction_update
       { time_offset = 8
       ; symbol = "AAPL"
       ; auction_type = 'O'
       ; reference_price = px "189.5"
       ; buy_shares = 500
       ; sell_shares = 300
       ; indicative_price = px "189.55"
       ; auction_only_price = px "189.6"
       });
  roundtrip
    (Auction_summary
       { time_offset = 9
       ; symbol = "AAPL"
       ; auction_type = 'O'
       ; price = px "189.55"
       ; shares = 300
       });
  roundtrip (End_of_session { time_offset = 10 });
  roundtrip (Time { seconds = 34_200 });
  [%expect
    {|
    type 0x2A length 41
      0: 29 2a 06 00 00 00 00 00 00 00
     10: 00 00 00 00 42 c8 00 00 00 4d
     20: 53 46 54 20 20 28 1a 40 00 00
     30: 00 00 00 0b 00 00 00 00 00 00
     40: 00
    type 0x31 length 18
      0: 12 31 07 00 00 00 4d 53 46 54
     10: 20 20 20 20 54 30 00 00
    type 0x95 length 47
      0: 2f 95 08 00 00 00 41 41 50 4c
     10: 20 20 20 20 4f 58 ea 1c 00 00
     20: 00 00 00 f4 01 00 00 2c 01 00
     30: 00 4c ec 1c 00 00 00 00 00 40
     40: ee 1c 00 00 00 00 00
    type 0x96 length 27
      0: 1b 96 09 00 00 00 41 41 50 4c
     10: 20 20 20 20 4f 4c ec 1c 00 00
     20: 00 00 00 2c 01 00 00
    type 0x2D length 6
      0: 06 2d 0a 00 00 00
    type 0x20 length 6
      0: 06 20 98 85 00 00
    |}]
;;

let%expect_test "options symbol mapping and spin messages" =
  roundtrip
    (Symbol_mapping
       { feed_symbol = "01SPYA"
       ; osi_symbol = "SPY   261218C00600000"
       ; symbol_condition = 'N'
       ; underlying = "SPY"
       });
  roundtrip (Spin_response { sequence = 42; order_count = 3; status = 'A' });
  roundtrip (Spin_finished { sequence = 42 });
  [%expect
    {|
    type 0x2E length 38
      0: 26 2e 30 31 53 50 59 41 53 50
     10: 59 20 20 20 32 36 31 32 31 38
     20: 43 30 30 36 30 30 30 30 30 4e
     30: 53 50 59 20 20 20 20 20
    type 0x82 length 11
      0: 0b 82 2a 00 00 00 03 00 00 00
     10: 41
    type 0x83 length 6
      0: 06 83 2a 00 00 00
    |}]
;;

let%expect_test "sequenced unit header framing" =
  let frame : Pitch.Frame.t =
    { unit = 1
    ; sequence = 17
    ; messages =
        [ Time { seconds = 34_200 }; Delete_order { time_offset = 5; order_id = 5 } ]
    }
  in
  let s = Pitch.Frame.to_string frame in
  dump s;
  print_s [%sexp (Pitch.Frame.of_string s : Pitch.Frame.t Or_error.t)];
  let heartbeat = Pitch.Frame.to_string { unit = 1; sequence = 19; messages = [] } in
  dump heartbeat;
  (* Unknown message types are skipped over by length. *)
  let unknown = "\x0c\x00\x01\x01\x01\x00\x00\x00\x04\x99\xaa\xbb" in
  print_s [%sexp (Pitch.Frame.of_string unknown : Pitch.Frame.t Or_error.t)];
  [%expect
    {|
      0: 1c 00 02 01 11 00 00 00 06 20
     10: 98 85 00 00 0e 29 05 00 00 00
     20: 05 00 00 00 00 00 00 00
    (Ok
     ((unit 1) (sequence 17)
      (messages
       ((Time (seconds 34200)) (Delete_order (time_offset 5) (order_id 5))))))
      0: 08 00 00 01 13 00 00 00
    (Ok
     ((unit 1) (sequence 1)
      (messages ((Unknown (message_type 153) (payload "\170\187"))))))
    |}]
;;
