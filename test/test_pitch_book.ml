open! Core
open Cboe_wire

let px = Price4.of_string

let add id side qty price : Pitch.Message.t =
  Add_order
    { time_offset = 0
    ; order_id = id
    ; side
    ; quantity = qty
    ; symbol = "AAPL"
    ; price = px price
    ; flags = 1
    }
;;

let show t =
  List.iter [ true; false ] ~f:(fun bids ->
    printf "%s:" (if bids then "bids" else "asks");
    List.iter (Pitch_book.levels t ~symbol:"AAPL" ~bids) ~f:(fun (p, q, n) ->
      printf " %s x %d (%d)" (Price4.to_string p) q n);
    print_endline "")
;;

let%expect_test "book from adds, executions, modifies and deletes" =
  let t = Pitch_book.create () in
  let apply = Pitch_book.apply t ~unit:1 in
  List.iter
    [ add 1 'B' 100 "229.99"
    ; add 2 'B' 200 "229.99"
    ; add 3 'B' 50 "229.98"
    ; add 4 'S' 100 "230.01"
    ; add 5 'S' 300 "230.02"
    ]
    ~f:apply;
  show t;
  apply
    (Order_executed
       { time_offset = 0
       ; order_id = 1
       ; executed_quantity = 100
       ; execution_id = 1
       ; trade_condition = None
       });
  apply (Reduce_size { time_offset = 0; order_id = 5; canceled_quantity = 100 });
  apply
    (Modify_order
       { time_offset = 0; order_id = 3; quantity = 70; price = px "230"; flags = 0 });
  apply (Delete_order { time_offset = 0; order_id = 4 });
  apply
    (Order_executed_at_price
       { time_offset = 0
       ; order_id = 2
       ; executed_quantity = 50
       ; remaining_quantity = 150
       ; execution_id = 2
       ; price = px "229.99"
       ; trade_condition = None
       });
  show t;
  print_s
    [%sexp (List.map (Pitch_book.orders t ~unit:1) ~f:(fun o -> o.order_id) : int list)];
  [%expect
    {|
    bids: 229.99 x 300 (2) 229.98 x 50 (1)
    asks: 230.01 x 100 (1) 230.02 x 300 (1)
    bids: 230 x 70 (1) 229.99 x 150 (1)
    asks: 230.02 x 200 (1)
    (2 3 5)
    |}]
;;
