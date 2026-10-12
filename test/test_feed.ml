open! Core
open Cboe_wire
open Cboe_sim

let day = Date.create_exn ~y:2026 ~m:Oct ~d:13

let print_frame label s =
  match Pitch.Frame.of_string s with
  | Error e -> print_s [%message label (e : Error.t)]
  | Ok f ->
    if List.is_empty f.messages
    then printf "%s [u%d #%d] heartbeat\n" label f.unit f.sequence;
    List.iter f.messages ~f:(fun m ->
      printf "%s [u%d #%d] %s\n" label f.unit f.sequence (Pretty.pitch m))
;;

let%expect_test "spin on connect, then sequenced live messages with Time" =
  let clock = Clock.manual (Clock.ny_time day "10:00:00.25") in
  let instruments = Instruments.universe ~symbols:[ "AAPL" ] () in
  let feed = Feed.create ~clock instruments in
  let venue =
    Venue.create
      ~config:{ Venue.Config.default with noise = false }
      ~clock
      ~emit_user:(fun ~user:_ ~unit:_ _ -> ())
      ~emit_feed:(Feed.publish feed)
      instruments
  in
  let order id side px =
    Venue.new_order
      venue
      ~user:"U"
      { (Boe.New_order.create ~cl_ord_id:id ~side ~order_qty:100 ~symbol:"AAPL" ()) with
        price = Some (Price4.of_string px)
      }
  in
  order "B1" '1' "229.90";
  order "S1" '2' "230.10";
  let sub =
    Feed.subscribe
      feed
      ~trading_status:(Venue.trading_status venue)
      ~send:(print_frame "a")
  in
  [%expect
    {|
    a [u1 #0] Time 10:00:00
    a [u1 #0] TradingStatus AAPL T regsho=0
    a [u1 #0] SpinResponse seq=3 orders=2 status=A
    a [u1 #0] AddOrder id=1 AAPL B 100@229.9
    a [u1 #0] AddOrder id=2 AAPL S 100@230.1
    a [u1 #0] SpinFinished seq=3
    a [u2 #0] Time 10:00:00
    a [u2 #0] SymbolMapping SPY001 -> "SPY   261218C00590000" (SPY)
    a [u2 #0] SymbolMapping SPY002 -> "SPY   261218C00600000" (SPY)
    a [u2 #0] SymbolMapping SPY003 -> "SPY   261218C00610000" (SPY)
    a [u2 #0] SymbolMapping SPY004 -> "SPY   261218P00590000" (SPY)
    a [u2 #0] SymbolMapping SPY005 -> "SPY   261218P00600000" (SPY)
    a [u2 #0] SymbolMapping SPY006 -> "SPY   261218P00610000" (SPY)
    a [u2 #0] TradingStatus SPY001 T regsho=0
    a [u2 #0] TradingStatus SPY002 T regsho=0
    a [u2 #0] TradingStatus SPY003 T regsho=0
    a [u2 #0] TradingStatus SPY004 T regsho=0
    a [u2 #0] TradingStatus SPY005 T regsho=0
    a [u2 #0] TradingStatus SPY006 T regsho=0
    a [u2 #0] SpinResponse seq=0 orders=0 status=A
    a [u2 #0] SpinFinished seq=0
    |}];
  Clock.set clock (Clock.ny_time day "10:00:01.5");
  order "B2" '1' "229.95";
  [%expect
    {|
    a [u1 #4] Time 10:00:01
    a [u1 #5] AddOrder id=3 AAPL B 100@229.95
    |}];
  let json = ref [] in
  let jsub =
    Feed.subscribe_json feed ~trading_status:(Venue.trading_status venue) ~send:(fun s ->
      json := s :: !json)
  in
  Venue.cancel_order venue ~user:"U" { orig_cl_ord_id = "B1" };
  List.iter (List.rev !json) ~f:print_endline;
  [%expect
    {|
    a [u1 #6] DeleteOrder id=1
    {"unit":1,"seq":0,"type":"Time","seconds":36001}
    {"unit":1,"seq":0,"type":"TradingStatus","time_offset":0,"symbol":"AAPL","status":"T","reg_sho_action":"0"}
    {"unit":1,"seq":0,"type":"SpinResponse","sequence":5,"order_count":3,"status":"A"}
    {"unit":1,"seq":0,"type":"AddOrder","time_offset":0,"order_id":1,"side":"B","quantity":100,"symbol":"AAPL","price":"229.9","flags":1}
    {"unit":1,"seq":0,"type":"AddOrder","time_offset":0,"order_id":2,"side":"S","quantity":100,"symbol":"AAPL","price":"230.1","flags":1}
    {"unit":1,"seq":0,"type":"AddOrder","time_offset":0,"order_id":3,"side":"B","quantity":100,"symbol":"AAPL","price":"229.95","flags":1}
    {"unit":1,"seq":0,"type":"SpinFinished","sequence":5}
    {"unit":2,"seq":0,"type":"Time","seconds":36001}
    {"unit":2,"seq":0,"type":"SymbolMapping","feed_symbol":"SPY001","osi_symbol":"SPY   261218C00590000","symbol_condition":"N","underlying":"SPY"}
    {"unit":2,"seq":0,"type":"SymbolMapping","feed_symbol":"SPY002","osi_symbol":"SPY   261218C00600000","symbol_condition":"N","underlying":"SPY"}
    {"unit":2,"seq":0,"type":"SymbolMapping","feed_symbol":"SPY003","osi_symbol":"SPY   261218C00610000","symbol_condition":"N","underlying":"SPY"}
    {"unit":2,"seq":0,"type":"SymbolMapping","feed_symbol":"SPY004","osi_symbol":"SPY   261218P00590000","symbol_condition":"N","underlying":"SPY"}
    {"unit":2,"seq":0,"type":"SymbolMapping","feed_symbol":"SPY005","osi_symbol":"SPY   261218P00600000","symbol_condition":"N","underlying":"SPY"}
    {"unit":2,"seq":0,"type":"SymbolMapping","feed_symbol":"SPY006","osi_symbol":"SPY   261218P00610000","symbol_condition":"N","underlying":"SPY"}
    {"unit":2,"seq":0,"type":"TradingStatus","time_offset":0,"symbol":"SPY001","status":"T","reg_sho_action":"0"}
    {"unit":2,"seq":0,"type":"TradingStatus","time_offset":0,"symbol":"SPY002","status":"T","reg_sho_action":"0"}
    {"unit":2,"seq":0,"type":"TradingStatus","time_offset":0,"symbol":"SPY003","status":"T","reg_sho_action":"0"}
    {"unit":2,"seq":0,"type":"TradingStatus","time_offset":0,"symbol":"SPY004","status":"T","reg_sho_action":"0"}
    {"unit":2,"seq":0,"type":"TradingStatus","time_offset":0,"symbol":"SPY005","status":"T","reg_sho_action":"0"}
    {"unit":2,"seq":0,"type":"TradingStatus","time_offset":0,"symbol":"SPY006","status":"T","reg_sho_action":"0"}
    {"unit":2,"seq":0,"type":"SpinResponse","sequence":0,"order_count":0,"status":"A"}
    {"unit":2,"seq":0,"type":"SpinFinished","sequence":0}
    {"unit":1,"seq":6,"type":"DeleteOrder","time_offset":500000000,"order_id":1}
    |}];
  Feed.unsubscribe feed sub;
  Feed.unsubscribe feed jsub;
  print_s [%sexp (Feed.subscriber_count feed : int)];
  [%expect {| 0 |}]
;;
