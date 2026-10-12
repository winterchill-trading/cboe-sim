open! Core
open Cboe_wire

let dump s =
  String.to_list s
  |> List.chunks_of ~length:10
  |> List.iteri ~f:(fun i row ->
    printf "%3d: %s\n" (i * 10) (Le.hex (String.of_char_list row)))
;;

let ack_bitfields =
  Bitfields.bitfields_of_names Bitfields.return [ "Side"; "Price"; "Symbol"; "OrderQty" ]
;;

let exec_bitfields =
  Bitfields.bitfields_of_names Bitfields.return [ "Symbol"; "LeavesQty"; "LastPx" ]
;;

let return_bitfields typ =
  if typ = Boe.Message_type.order_acknowledgment
  then ack_bitfields
  else if typ = Boe.Message_type.order_execution
  then exec_bitfields
  else ""
;;

let return : Boe.Return_fields.t =
  { Boe.Return_fields.empty with
    side = Some '1'
  ; price = Some (Price4.of_string "189.5")
  ; symbol = Some "AAPL"
  ; order_qty = Some 100
  ; leaves_qty = Some 60
  ; last_px = Some (Price4.of_string "189.5")
  }
;;

(* Drops [(field ())] entries so that only the optional fields present are shown. *)
let rec drop_absent : Sexp.t -> Sexp.t = function
  | Atom _ as a -> a
  | List l ->
    List
      (List.filter_map l ~f:(function
        | List [ Atom _; List [] ] -> None
        | x -> Some (drop_absent x)))
;;

let outbound ?(matching_unit = 1) ?(sequence = 3) m =
  let s = Boe.Outbound.to_string ~matching_unit ~sequence ~return_bitfields m in
  dump s;
  let u, seq, back = Boe.Outbound.of_string s |> Or_error.ok_exn in
  print_s (drop_absent [%message (u : int) (seq : int) (back : Boe.Outbound.t)])
;;

let%expect_test "order acknowledgment with selected return fields" =
  outbound
    (Order_acknowledgment
       { transaction_time = 1_760_000_000_000_000_000
       ; cl_ord_id = "ORD1"
       ; order_id = 42
       ; return
       });
  (* TransactionTime @10, ClOrdID @18, OrderID @38, reserved @46, count @47; only the
     selected Side, Price, Symbol and OrderQty come back, in bit order. *)
  [%expect
    {|
      0: ba ba 46 00 25 01 03 00 00 00
     10: 00 00 b0 d4 ac c6 6c 18 4f 52
     20: 44 31 00 00 00 00 00 00 00 00
     30: 00 00 00 00 00 00 00 00 2a 00
     40: 00 00 00 00 00 00 00 03 05 01
     50: 40 31 58 ea 1c 00 00 00 00 00
     60: 41 41 50 4c 00 00 00 00 64 00
     70: 00 00
    ((u 1) (seq 3)
     (back
      (Order_acknowledgment (transaction_time 1760000000000000000)
       (cl_ord_id ORD1) (order_id 42)
       (return ((side (1)) (price (1895000)) (symbol (AAPL)) (order_qty (100)))))))
    |}]
;;

let%expect_test "execution, cancel and rejects" =
  outbound
    (Order_execution
       { transaction_time = 1
       ; cl_ord_id = "ORD1"
       ; exec_id = 7
       ; last_shares = 40
       ; last_px = Price4.of_string "189.5"
       ; leaves_qty = 60
       ; base_liquidity_indicator = 'A'
       ; sub_liquidity_indicator = '\000'
       ; contra_broker = "CBOE"
       ; return
       });
  outbound
    (Order_cancelled { transaction_time = 2; cl_ord_id = "ORD1"; reason = 'U'; return });
  (* Rejects are unsequenced: unit and sequence are written as zero. *)
  outbound
    (Order_rejected
       { transaction_time = 3
       ; cl_ord_id = "ORD2"
       ; reason = 'Y'
       ; text = "symbol not supported"
       ; return = Boe.Return_fields.empty
       });
  [%expect
    {|
      0: ba ba 5d 00 2c 01 03 00 00 00
     10: 01 00 00 00 00 00 00 00 4f 52
     20: 44 31 00 00 00 00 00 00 00 00
     30: 00 00 00 00 00 00 00 00 07 00
     40: 00 00 00 00 00 00 28 00 00 00
     50: 58 ea 1c 00 00 00 00 00 3c 00
     60: 00 00 41 00 43 42 4f 45 00 05
     70: 00 01 00 00 0a 41 41 50 4c 00
     80: 00 00 00 3c 00 00 00 58 ea 1c
     90: 00 00 00 00 00
    ((u 1) (seq 3)
     (back
      (Order_execution (transaction_time 1) (cl_ord_id ORD1) (exec_id 7)
       (last_shares 40) (last_px 1895000) (leaves_qty 60)
       (base_liquidity_indicator A) (sub_liquidity_indicator "\000")
       (contra_broker CBOE)
       (return ((symbol (AAPL)) (leaves_qty (60)) (last_px (1895000)))))))
      0: ba ba 27 00 2a 01 03 00 00 00
     10: 02 00 00 00 00 00 00 00 4f 52
     20: 44 31 00 00 00 00 00 00 00 00
     30: 00 00 00 00 00 00 00 00 55 00
     40: 00
    ((u 1) (seq 3)
     (back
      (Order_cancelled (transaction_time 2) (cl_ord_id ORD1) (reason U)
       (return ()))))
      0: ba ba 63 00 26 00 00 00 00 00
     10: 03 00 00 00 00 00 00 00 4f 52
     20: 44 32 00 00 00 00 00 00 00 00
     30: 00 00 00 00 00 00 00 00 59 73
     40: 79 6d 62 6f 6c 20 6e 6f 74 20
     50: 73 75 70 70 6f 72 74 65 64 00
     60: 00 00 00 00 00 00 00 00 00 00
     70: 00 00 00 00 00 00 00 00 00 00
     80: 00 00 00 00 00 00 00 00 00 00
     90: 00 00 00 00 00 00 00 00 00 00
    100: 00
    ((u 0) (seq 0)
     (back
      (Order_rejected (transaction_time 3) (cl_ord_id ORD2) (reason Y)
       (text "symbol not supported") (return ()))))
    |}]
;;

let%expect_test "session messages" =
  outbound
    (Login_response
       { status = 'A'
       ; text = "accepted"
       ; no_unspecified_unit_replay = false
       ; last_received_sequence = 12
       ; units = [ 1, 30; 2, 4 ]
       ; return_bitfields = [ Boe.Message_type.order_acknowledgment, ack_bitfields ]
       });
  outbound Server_heartbeat;
  outbound Replay_complete;
  outbound
    (Logout { reason = 'U'; text = "bye"; last_received_sequence = 12; units = [ 1, 30 ] });
  [%expect
    {|
      0: ba ba 5f 00 24 00 00 00 00 00
     10: 41 61 63 63 65 70 74 65 64 00
     20: 00 00 00 00 00 00 00 00 00 00
     30: 00 00 00 00 00 00 00 00 00 00
     40: 00 00 00 00 00 00 00 00 00 00
     50: 00 00 00 00 00 00 00 00 00 00
     60: 00 00 00 00 00 00 00 00 00 00
     70: 00 00 0c 00 00 00 02 01 1e 00
     80: 00 00 02 04 00 00 00 01 08 00
     90: 81 25 03 05 01 40 00
    ((u 0) (seq 0)
     (back
      (Login_response
       ((status A) (text accepted) (no_unspecified_unit_replay false)
        (last_received_sequence 12) (units ((1 30) (2 4)))
        (return_bitfields ((37 "\005\001@")))))))
      0: ba ba 08 00 09 00 00 00 00 00
    ((u 0) (seq 0) (back Server_heartbeat))
      0: ba ba 08 00 13 00 00 00 00 00
    ((u 0) (seq 0) (back Replay_complete))
      0: ba ba 4f 00 08 00 00 00 00 00
     10: 55 62 79 65 00 00 00 00 00 00
     20: 00 00 00 00 00 00 00 00 00 00
     30: 00 00 00 00 00 00 00 00 00 00
     40: 00 00 00 00 00 00 00 00 00 00
     50: 00 00 00 00 00 00 00 00 00 00
     60: 00 00 00 00 00 00 00 00 00 00
     70: 00 0c 00 00 00 01 01 1e 00 00
     80: 00
    ((u 0) (seq 0)
     (back
      (Logout
       ((reason U) (text bye) (last_received_sequence 12) (units ((1 30)))))))
    |}]
;;

let%expect_test "execution ids print as 9 base-36 characters" =
  List.iter
    [ 0; 7; 35; 36; (12 * Int.pow 36 8) + 1 ]
    ~f:(fun id -> print_endline (Boe.exec_id_to_string id));
  [%expect
    {|
    000000000
    000000007
    00000000Z
    000000010
    C00000001
    |}]
;;
