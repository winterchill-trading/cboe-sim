open! Core
open Cboe_wire
open Cboe_sim

let day = Date.create_exn ~y:2026 ~m:Oct ~d:13
let wall = ref (Time_ns.of_int_ns_since_epoch 0)

let setup () =
  let clock = Clock.manual (Clock.ny_time day "10:00:00") in
  let store = Boe_session.Store.create () in
  let venue =
    Venue.create
      ~config:{ Venue.Config.default with noise = false }
      ~clock
      ~emit_user:(Boe_session.Store.deliver store)
      ~emit_feed:(fun ~unit:_ _ -> ())
      (Instruments.universe ())
  in
  store, venue
;;

let connect (store, venue) ~name =
  Boe_session.Handler.create
    ~store
    ~venue
    ~now:(fun () -> !wall)
    ~send:(fun frame ->
      match Boe.Outbound.of_string frame with
      | Ok (unit, seq, m) ->
        printf "%s< [u%d #%d] %s\n" name unit seq (Pretty.boe_outbound m)
      | Error e -> print_s [%message name (e : Error.t)])
    ~close:(fun () -> printf "%s: closed\n" name)
    ()
;;

let login ?unit_sequences ?(return_bitfields = []) ?(user = "TEST") h =
  Boe_session.Handler.on_frame
    h
    (Boe.Inbound.to_string
       ~sequence:0
       (Login_request
          { session_sub_id = "0001"
          ; username = user
          ; password = "pw"
          ; unit_sequences
          ; return_bitfields
          }))
;;

let new_order h ~sequence id =
  Boe_session.Handler.on_frame
    h
    (Boe.Inbound.to_string
       ~sequence
       (New_order
          { (Boe.New_order.create
               ~cl_ord_id:id
               ~side:'1'
               ~order_qty:100
               ~symbol:"AAPL"
               ())
            with
            price = Some (Price4.of_string "229.5")
          }))
;;

let%expect_test "login, orders, duplicate sequence, logout, replay on reconnect" =
  let env = setup () in
  let h = connect env ~name:"c1" in
  login h;
  new_order h ~sequence:1 "A1";
  new_order h ~sequence:2 "A2";
  (* A resend of sequence 2 after a reconnect is ignored. *)
  new_order h ~sequence:2 "A2";
  [%expect
    {|
    c1< [u0 #0] LoginResponse status=A text="accepted" last_seq=0 units=1:0,2:0
    c1< [u0 #0] ReplayComplete
    c1< [u1 #1] OrderAck A1 order_id=1
    c1< [u1 #2] OrderAck A2 order_id=2
    |}];
  let h2 = connect env ~name:"c2" in
  login h2;
  [%expect
    {|
    c2< [u0 #0] LoginResponse status=B text="session in use" last_seq=0 units=
    c2: closed
    |}];
  Boe_session.Handler.on_frame h (Boe.Inbound.to_string ~sequence:0 Logout_request);
  [%expect {|
    c1< [u0 #0] Logout reason=U text="user requested"
    c1: closed
    |}];
  let h3 = connect env ~name:"c3" in
  login h3 ~unit_sequences:{ no_unspecified_unit_replay = false; units = [ 1, 1 ] };
  new_order h3 ~sequence:3 "A3";
  [%expect
    {|
    c3< [u0 #0] LoginResponse status=A text="accepted" last_seq=2 units=1:2,2:0
    c3< [u1 #2] OrderAck A2 order_id=2
    c3< [u0 #0] ReplayComplete
    c3< [u1 #3] OrderAck A3 order_id=3
    |}]
;;

let%expect_test "invalid return bitfields, login required, heartbeats and timeout" =
  let env = setup () in
  let h = connect env ~name:"c1" in
  login h ~return_bitfields:[ Boe.Message_type.order_acknowledgment, "\000\128" ];
  let h2 = connect env ~name:"c2" in
  new_order h2 ~sequence:1 "X";
  let h3 = connect env ~name:"c3" in
  login h3 ~user:"HB";
  wall := Time_ns.add !wall (Time_ns.Span.of_sec 1.);
  Boe_session.Handler.on_timer h3;
  wall := Time_ns.add !wall (Time_ns.Span.of_sec 5.);
  Boe_session.Handler.on_timer h3;
  [%expect
    {|
    c1< [u0 #0] LoginResponse status=F text="invalid return bitfields for 0x25" last_seq=0 units=
    c1: closed
    c2< [u0 #0] Logout reason=! text="login required"
    c2: closed
    c3< [u0 #0] LoginResponse status=A text="accepted" last_seq=0 units=1:0,2:0
    c3< [u0 #0] ReplayComplete
    c3< [u0 #0] ServerHeartbeat
    c3< [u0 #0] Logout reason=! text="heartbeat timeout"
    c3: closed
    |}]
;;
