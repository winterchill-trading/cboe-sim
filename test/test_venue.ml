open! Core
open Cboe_wire
open Cboe_sim

let day = Date.create_exn ~y:2026 ~m:Oct ~d:13

let venue ?(at = "10:00:00") () =
  let clock = Clock.manual (Clock.ny_time day at) in
  let v =
    Venue.create
      ~config:{ Venue.Config.default with noise = false }
      ~clock
      ~emit_user:(fun ~user ~unit:_ m -> printf "%s< %s\n" user (Pretty.boe_outbound m))
      ~emit_feed:(fun ~unit m -> printf "pitch%d> %s\n" unit (Pretty.pitch m))
      (Instruments.universe ())
  in
  v, clock
;;

let order
  ?(symbol = "AAPL")
  ?price
  ?tif
  ?ord_type
  ?max_floor
  ?display_indicator
  ?routing_inst
  ?prevent_match
  ~id
  ~side
  qty
  =
  { (Boe.New_order.create ~cl_ord_id:id ~side ~order_qty:qty ~symbol ()) with
    price = Option.map price ~f:Price4.of_string
  ; time_in_force = tif
  ; ord_type
  ; max_floor
  ; display_indicator
  ; routing_inst
  ; prevent_match
  }
;;

let%expect_test "resting order, fill against another member, executions on both sides" =
  let v, _ = venue () in
  Venue.new_order v ~user:"AAAA" (order ~id:"B1" ~side:'1' ~price:"229.50" 300);
  [%expect
    {|
    AAAA< OrderAck B1 order_id=1 sym=AAPL side=1 px=229.5 qty=300 leaves=300
    pitch1> AddOrder id=1 AAPL B 300@229.5
    |}];
  Venue.new_order v ~user:"BBBB" (order ~id:"S1" ~side:'2' ~price:"229.40" 100);
  [%expect
    {|
    BBBB< OrderAck S1 order_id=2 sym=AAPL side=2 px=229.4 qty=100 leaves=100
    pitch1> OrderExecuted id=1 qty=100 exec=000000001
    BBBB< OrderExecution S1 exec=000000001 100@229.5 leaves=0 liq=R
    AAAA< OrderExecution B1 exec=000000001 100@229.5 leaves=200 liq=A
    |}];
  Venue.new_order v ~user:"BBBB" (order ~id:"S2" ~side:'2' ~price:"229.60" 50);
  Venue.new_order v ~user:"CCCC" (order ~id:"X1" ~side:'1' ~price:"229.70" ~tif:'3' 100);
  [%expect
    {|
    BBBB< OrderAck S2 order_id=3 sym=AAPL side=2 px=229.6 qty=50 leaves=50
    pitch1> AddOrder id=3 AAPL S 50@229.6
    CCCC< OrderAck X1 order_id=4 sym=AAPL side=1 px=229.7 qty=100 leaves=100
    pitch1> OrderExecuted id=3 qty=50 exec=000000002
    CCCC< OrderExecution X1 exec=000000002 50@229.6 leaves=50 liq=R
    BBBB< OrderExecution S2 exec=000000002 50@229.6 leaves=0 liq=A
    CCCC< OrderCancelled X1 reason=N sym=AAPL side=1 px=229.7 qty=100 leaves=50
    |}]
;;

let%expect_test "rejects with venue reason codes" =
  let v, _ = venue () in
  Venue.new_order v ~user:"AAAA" (order ~symbol:"ZZZZ" ~id:"R1" ~side:'1' ~price:"1" 1);
  Venue.new_order v ~user:"AAAA" (order ~id:"R2" ~side:'1' ~price:"229.505" 100);
  Venue.new_order v ~user:"AAAA" (order ~id:"R3" ~side:'1' ~price:"229.5" 1_000_000);
  Venue.new_order v ~user:"AAAA" (order ~id:"R4" ~side:'1' ~price:"300" 100);
  Venue.new_order v ~user:"AAAA" (order ~id:"R5" ~side:'1' ~price:"229.5" ~tif:'6' 100);
  Venue.new_order v ~user:"AAAA" (order ~id:"R6" ~side:'1' ~price:"229.5" 100);
  Venue.new_order v ~user:"AAAA" (order ~id:"R6" ~side:'1' ~price:"229.5" 100);
  Venue.new_order
    v
    ~user:"BBBB"
    (order ~id:"P1" ~side:'2' ~price:"229.5" ~routing_inst:"P" 100);
  Venue.new_order v ~user:"BBBB" (order ~id:"I1" ~side:'2' ~price:"231" ~tif:'3' 100);
  Venue.new_order v ~user:"BBBB" (order ~id:"F1" ~side:'2' ~price:"229" ~tif:'4' 200);
  [%expect
    {|
    AAAA< OrderRejected R1 reason=Y "symbol not supported"
    AAAA< OrderRejected R2 reason=A "Price not on the tick"
    AAAA< OrderRejected R3 reason=M "OrderQty above 999,999"
    AAAA< OrderRejected R4 reason=u "price outside the LULD bands"
    AAAA< OrderRejected R5 reason=A "unsupported TimeInForce '6'"
    AAAA< OrderAck R6 order_id=1 sym=AAPL side=1 px=229.5 qty=100 leaves=100
    pitch1> AddOrder id=1 AAPL B 100@229.5
    AAAA< OrderRejected R6 reason=D "duplicate ClOrdID"
    BBBB< OrderRejected P1 reason=W "post only order would remove liquidity"
    BBBB< OrderAck I1 order_id=3 sym=AAPL side=2 px=231 qty=100 leaves=100
    BBBB< OrderCancelled I1 reason=N sym=AAPL side=2 px=231 qty=100 leaves=100
    BBBB< OrderAck F1 order_id=1000000000 sym=AAPL side=2 px=229 qty=200 leaves=200
    BBBB< OrderCancelled F1 reason=N sym=AAPL side=2 px=229 qty=200 leaves=200
    |}]
;;

let%expect_test "modify keeps the order id; cancel and too-late cancel" =
  let v, _ = venue () in
  Venue.new_order v ~user:"AAAA" (order ~id:"B1" ~side:'1' ~price:"229.50" 300);
  Venue.modify_order
    v
    ~user:"AAAA"
    { cl_ord_id = "B2"
    ; orig_cl_ord_id = "B1"
    ; order_qty = Some 200
    ; price = None
    ; ord_type = None
    ; cancel_orig_on_reject = None
    ; side = None
    ; max_floor = None
    };
  Venue.modify_order
    v
    ~user:"AAAA"
    { cl_ord_id = "B3"
    ; orig_cl_ord_id = "B2"
    ; order_qty = None
    ; price = Some (Price4.of_string "229.60")
    ; ord_type = None
    ; cancel_orig_on_reject = None
    ; side = None
    ; max_floor = None
    };
  Venue.cancel_order v ~user:"AAAA" { orig_cl_ord_id = "B3" };
  Venue.cancel_order v ~user:"AAAA" { orig_cl_ord_id = "B3" };
  Venue.cancel_order v ~user:"AAAA" { orig_cl_ord_id = "NOPE" };
  [%expect
    {|
    AAAA< OrderAck B1 order_id=1 sym=AAPL side=1 px=229.5 qty=300 leaves=300
    pitch1> AddOrder id=1 AAPL B 300@229.5
    AAAA< OrderModified B2 order_id=1 sym=AAPL side=1 px=229.5 qty=200 leaves=200 orig=B1
    pitch1> ReduceSize id=1 by=100
    AAAA< OrderModified B3 order_id=1 sym=AAPL side=1 px=229.6 qty=200 leaves=200 orig=B2
    pitch1> ModifyOrder id=1 200@229.6
    pitch1> DeleteOrder id=1
    AAAA< OrderCancelled B3 reason=U sym=AAPL side=1 px=229.6 qty=200 leaves=200 orig=B2
    AAAA< CancelRejected B3 reason=J "too late to cancel"
    AAAA< CancelRejected NOPE reason=O "unknown ClOrdID"
    |}]
;;

let%expect_test "hidden orders print trades, reserve orders reload with a new id" =
  let v, _ = venue () in
  Venue.new_order
    v
    ~user:"AAAA"
    (order ~id:"H1" ~side:'2' ~price:"230.10" ~display_indicator:'I' 100);
  Venue.new_order
    v
    ~user:"AAAA"
    (order ~id:"R1" ~side:'2' ~price:"230.20" ~max_floor:100 250);
  [%expect
    {|
    AAAA< OrderAck H1 order_id=1 sym=AAPL side=2 px=230.1 qty=100 leaves=100
    AAAA< OrderAck R1 order_id=2 sym=AAPL side=2 px=230.2 qty=250 leaves=250
    pitch1> AddOrder id=2 AAPL S 100@230.2
    |}];
  Venue.new_order v ~user:"BBBB" (order ~id:"T1" ~side:'1' ~price:"230.20" ~tif:'3' 250);
  [%expect
    {|
    BBBB< OrderAck T1 order_id=3 sym=AAPL side=1 px=230.2 qty=250 leaves=250
    pitch1> Trade id=0 AAPL 100@230.1 exec=000000001
    BBBB< OrderExecution T1 exec=000000001 100@230.1 leaves=150 liq=R
    AAAA< OrderExecution H1 exec=000000001 100@230.1 leaves=0 liq=A
    pitch1> OrderExecuted id=2 qty=100 exec=000000002
    BBBB< OrderExecution T1 exec=000000002 100@230.2 leaves=50 liq=R
    AAAA< OrderExecution R1 exec=000000002 100@230.2 leaves=150 liq=A
    BBBB< OrderCancelled T1 reason=N sym=AAPL side=1 px=230.2 qty=250 leaves=50
    AAAA< OrderRestated R1 order_id=4 reason=R sym=AAPL side=2 px=230.2 qty=250 leaves=150
    pitch1> AddOrder id=4 AAPL S 100@230.2
    |}]
;;

let%expect_test "self-trade prevention: cancel both" =
  let v, _ = venue () in
  Venue.new_order v ~user:"AAAA" (order ~id:"S1" ~side:'2' ~price:"230" 100);
  Venue.new_order
    v
    ~user:"AAAA"
    (order ~id:"B1" ~side:'1' ~price:"230" ~prevent_match:"BF" 100);
  [%expect
    {|
    AAAA< OrderAck S1 order_id=1 sym=AAPL side=2 px=230 qty=100 leaves=100
    pitch1> AddOrder id=1 AAPL S 100@230
    pitch1> DeleteOrder id=1
    AAAA< OrderCancelled S1 reason=V sym=AAPL side=2 px=230 qty=100 leaves=100
    AAAA< OrderAck B1 order_id=1000000000 sym=AAPL side=1 px=230 qty=100 leaves=100
    AAAA< OrderCancelled B1 reason=V sym=AAPL side=1 px=230 qty=100 leaves=100
    |}]
;;

let%expect_test "options by feed symbol and by OSI fields" =
  let v, _ = venue () in
  Venue.new_order
    v
    ~user:"AAAA"
    (order ~symbol:"SPY002" ~id:"O1" ~side:'1' ~price:"17.95" 10);
  Venue.new_order
    v
    ~user:"BBBB"
    { (order ~symbol:"SPY" ~id:"O2" ~side:'2' ~price:"17.95" 4) with
      maturity_date = Some 20261218
    ; strike_price = Some (Price4.of_string "600")
    ; put_or_call = Some '1'
    };
  [%expect
    {|
    AAAA< OrderAck O1 order_id=1 sym=SPY002 side=1 px=17.95 qty=10 leaves=10
    pitch2> AddOrder id=1 SPY002 B 10@17.95
    BBBB< OrderAck O2 order_id=2 sym=SPY002 side=2 px=17.95 qty=4 leaves=4
    pitch2> OrderExecuted id=1 qty=4 exec=000000001
    BBBB< OrderExecution O2 exec=000000001 4@17.95 leaves=0 liq=R
    AAAA< OrderExecution O1 exec=000000001 4@17.95 leaves=6 liq=A
    |}]
;;

let%expect_test "opening auction uncrosses on-open orders with the book" =
  let v, clock = venue ~at:"09:29:00" () in
  Venue.new_order v ~user:"AAAA" (order ~id:"B1" ~side:'1' ~price:"229.90" 200);
  Venue.new_order v ~user:"BBBB" (order ~id:"M1" ~side:'2' ~tif:'2' ~ord_type:'1' 150);
  Venue.new_order v ~user:"CCCC" (order ~id:"L1" ~side:'2' ~tif:'2' ~price:"229.80" 100);
  Venue.new_order v ~user:"CCCC" (order ~id:"C1" ~side:'1' ~tif:'7' ~ord_type:'1' 10);
  Venue.tick v;
  [%expect
    {|
    AAAA< OrderAck B1 order_id=1 sym=AAPL side=1 px=229.9 qty=200 leaves=200
    pitch1> AddOrder id=1 AAPL B 200@229.9
    BBBB< OrderAck M1 order_id=1000000000 sym=AAPL side=2 qty=150 leaves=150
    CCCC< OrderAck L1 order_id=1000000001 sym=AAPL side=2 px=229.8 qty=100 leaves=100
    CCCC< OrderAck C1 order_id=1000000002 sym=AAPL side=1 qty=10 leaves=10
    pitch1> AuctionUpdate AAPL O ref=230 buy=0 sell=250 indicative=229.9 auction_only=0
    |}];
  Clock.set clock (Clock.ny_time day "09:30:00");
  Venue.tick v;
  [%expect
    {|
    pitch1> OrderExecutedAtPrice id=1 qty=200@229.9 remaining=0 exec=C00000001
    AAAA< OrderExecution B1 exec=C00000001 200@229.9 leaves=0 liq=C
    BBBB< OrderExecution M1 exec=C00000002 150@229.9 leaves=0 liq=C
    CCCC< OrderExecution L1 exec=C00000003 50@229.9 leaves=50 liq=C
    pitch1> AuctionSummary AAPL O 200@229.9
    CCCC< OrderCancelled L1 reason=X sym=AAPL side=2 px=229.8 qty=100 leaves=50
    |}]
;;

let%expect_test "close: closing auction, day orders expire, end of session" =
  let v, clock = venue ~at:"15:59:59" () in
  Venue.new_order v ~user:"AAAA" (order ~id:"B1" ~side:'1' ~price:"229.90" 100);
  Venue.new_order v ~user:"BBBB" (order ~id:"C1" ~side:'2' ~tif:'7' ~ord_type:'1' 60);
  Venue.new_order v ~user:"BBBB" (order ~id:"M1" ~side:'2' ~ord_type:'1' 10);
  [%expect
    {|
    AAAA< OrderAck B1 order_id=1 sym=AAPL side=1 px=229.9 qty=100 leaves=100
    pitch1> AddOrder id=1 AAPL B 100@229.9
    BBBB< OrderAck C1 order_id=1000000000 sym=AAPL side=2 qty=60 leaves=60
    BBBB< OrderAck M1 order_id=2 sym=AAPL side=2 qty=10 leaves=10
    pitch1> OrderExecuted id=1 qty=10 exec=000000001
    BBBB< OrderExecution M1 exec=000000001 10@229.9 leaves=0 liq=R
    AAAA< OrderExecution B1 exec=000000001 10@229.9 leaves=90 liq=A
    |}];
  Clock.set clock (Clock.ny_time day "16:00:00");
  Venue.tick v;
  Venue.new_order v ~user:"AAAA" (order ~id:"B2" ~side:'1' ~ord_type:'1' 10);
  Venue.new_order v ~user:"AAAA" (order ~id:"B3" ~side:'1' ~price:"229" 10);
  [%expect
    {|
    pitch1> OrderExecutedAtPrice id=1 qty=60@229.9 remaining=30 exec=C00000001
    AAAA< OrderExecution B1 exec=C00000001 60@229.9 leaves=30 liq=C
    BBBB< OrderExecution C1 exec=C00000002 60@229.9 leaves=0 liq=C
    pitch1> AuctionSummary AAPL C 60@229.9
    pitch1> DeleteOrder id=1
    AAAA< OrderCancelled B1 reason=X sym=AAPL side=1 px=229.9 qty=100 leaves=30
    AAAA< OrderRejected B2 reason=A "market orders are accepted in the core session only"
    AAAA< OrderAck B3 order_id=4 sym=AAPL side=1 px=229 qty=10 leaves=10
    pitch1> AddOrder id=4 AAPL B 10@229
    |}];
  Clock.set clock (Clock.ny_time day "20:00:00");
  Venue.tick v;
  Venue.new_order v ~user:"AAAA" (order ~id:"B4" ~side:'1' ~price:"229" 10);
  [%expect
    {|
    pitch1> DeleteOrder id=4
    AAAA< OrderCancelled B3 reason=X sym=AAPL side=1 px=229 qty=10 leaves=10
    pitch1> EndOfSession
    pitch2> EndOfSession
    AAAA< OrderRejected B4 reason=A "market closed"
    |}]
;;
