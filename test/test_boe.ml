open! Core
open Cboe_wire

(* Hex dump in rows of 10 bytes with the offset of each row, to compare with the
   specification's offset tables. *)
let dump s =
  String.to_list s
  |> List.chunks_of ~length:10
  |> List.iteri ~f:(fun i row ->
    printf "%3d: %s\n" (i * 10) (Le.hex (String.of_char_list row)))
;;

let inbound ?(sequence = 0) m =
  let s = Boe.Inbound.to_string ~sequence m in
  dump s;
  let seq, back = Boe.Inbound.of_string s |> Or_error.ok_exn in
  if seq <> sequence || not (Boe.Inbound.equal back m)
  then print_s [%message "round trip mismatch" (back : Boe.Inbound.t)]
;;

let%expect_test "client heartbeat and logout request are bare headers" =
  inbound Client_heartbeat;
  inbound Logout_request;
  [%expect
    {|
      0: ba ba 08 00 03 00 00 00 00 00
      0: ba ba 08 00 02 00 00 00 00 00
    |}]
;;

let%expect_test "login request with unit sequences and return bitfields" =
  inbound
    (Login_request
       { session_sub_id = "0001"
       ; username = "TEST"
       ; password = "secret"
       ; unit_sequences = Some { no_unspecified_unit_replay = false; units = [ 1, 5 ] }
       ; return_bitfields =
           [ ( Boe.Message_type.order_acknowledgment
             , Bitfields.bitfields_of_names Bitfields.return [ "Side"; "Price"; "Symbol" ]
             )
           ]
       });
  (* SessionSubID @10, Username @14, Password @18, NumberOfParamGroups @28, then the 0x80
     group (length 10) and the 0x81 group (length 7). *)
  [%expect
    {|
      0: ba ba 2c 00 37 00 00 00 00 00
     10: 30 30 30 31 54 45 53 54 73 65
     20: 63 72 65 74 00 00 00 00 02 0a
     30: 00 80 00 01 01 05 00 00 00 07
     40: 00 81 25 02 05 01
    |}]
;;

let%expect_test "new order: fixed part and optional fields in bit order" =
  inbound
    ~sequence:7
    (New_order
       { (Boe.New_order.create
            ~cl_ord_id:"ORD1"
            ~side:'1'
            ~order_qty:100
            ~symbol:"AAPL"
            ())
         with
         price = Some (Price4.of_string "189.50")
       ; time_in_force = Some '0'
       });
  (* ClOrdID @10 (20), Side @30, OrderQty @31, NumberOfNewOrderBitfields @35 = 2,
     bitfields 0x24 (Price, TimeInForce) 0x41 (Symbol, Capacity), then Price (8),
     TimeInForce (1), Symbol (8), Capacity (1). *)
  [%expect
    {|
     0: ba ba 36 00 38 00 07 00 00 00
    10: 4f 52 44 31 00 00 00 00 00 00
    20: 00 00 00 00 00 00 00 00 00 00
    30: 31 64 00 00 00 02 24 41 58 ea
    40: 1c 00 00 00 00 00 30 41 41 50
    50: 4c 00 00 00 00 41
    |}]
;;

let%expect_test "cancel and modify" =
  inbound ~sequence:8 (Cancel_order { orig_cl_ord_id = "ORD1" });
  inbound
    ~sequence:9
    (Modify_order
       { cl_ord_id = "ORD2"
       ; orig_cl_ord_id = "ORD1"
       ; order_qty = Some 50
       ; price = Some (Price4.of_string "189.4")
       ; ord_type = None
       ; cancel_orig_on_reject = None
       ; side = None
       ; max_floor = None
       });
  [%expect
    {|
     0: ba ba 1d 00 39 00 08 00 00 00
    10: 4f 52 44 31 00 00 00 00 00 00
    20: 00 00 00 00 00 00 00 00 00 00
    30: 00
     0: ba ba 3e 00 3a 00 09 00 00 00
    10: 4f 52 44 32 00 00 00 00 00 00
    20: 00 00 00 00 00 00 00 00 00 00
    30: 4f 52 44 31 00 00 00 00 00 00
    40: 00 00 00 00 00 00 00 00 00 00
    50: 01 0c 32 00 00 00 70 e6 1c 00
    60: 00 00 00 00
    |}]
;;

let%expect_test "decode errors" =
  let show s =
    print_s [%sexp (Boe.Inbound.of_string s : (int * Boe.Inbound.t) Or_error.t)]
  in
  show "\xba\xbb\x08\x00\x03\x00\x00\x00\x00\x00";
  show "\xba\xba\x08\x00\x77\x00\x00\x00\x00\x00";
  show "\xba\xba\x09\x00\x38\x00\x00\x00\x00\x00\x00";
  [%expect
    {|
    (Error "BOE frame does not start with 0xBABA")
    (Error ("unknown inbound message type" (typ 119)))
    (Error ("message too short" (name NewOrder) (len 11)))
    |}]
;;
