open! Core
open Cboe_wire

type sink =
  | Binary of (string -> unit)
  | Json of (string -> unit)

type subscription = int

type t =
  { clock : Clock.t
  ; instruments : Instruments.t list
  ; seqs : int Int.Table.t (** last sequence per unit *)
  ; last_time : int Int.Table.t (** seconds of the last Time message per unit *)
  ; last_sent : Time_ns.t Int.Table.t (** wall time of the last frame per unit *)
  ; book : Pitch_book.t
  ; subscribers : sink Int.Table.t
  ; mutable next_subscriber : int
  }

let units = [ Instruments.equities_unit; Instruments.options_unit ]

let create ~clock instruments =
  { clock
  ; instruments
  ; seqs = Int.Table.create ()
  ; last_time = Int.Table.create ()
  ; last_sent = Int.Table.create ()
  ; book = Pitch_book.create ()
  ; subscribers = Int.Table.create ()
  ; next_subscriber = 0
  }
;;

let book t = t.book
let next_sequence t ~unit = 1 + Option.value (Hashtbl.find t.seqs unit) ~default:0
let subscriber_count t = Hashtbl.length t.subscribers

let with_offset (m : Pitch.Message.t) time_offset : Pitch.Message.t =
  match m with
  | Add_order r -> Add_order { r with time_offset }
  | Order_executed r -> Order_executed { r with time_offset }
  | Order_executed_at_price r -> Order_executed_at_price { r with time_offset }
  | Reduce_size r -> Reduce_size { r with time_offset }
  | Modify_order r -> Modify_order { r with time_offset }
  | Delete_order r -> Delete_order { r with time_offset }
  | Trade r -> Trade { r with time_offset }
  | End_of_session _ -> End_of_session { time_offset }
  | Trading_status r -> Trading_status { r with time_offset }
  | Auction_update r -> Auction_update { r with time_offset }
  | Auction_summary r -> Auction_summary { r with time_offset }
  | Time _ | Symbol_mapping _ | Spin_response _ | Spin_finished _ | Unknown _ -> m
;;

let price p = `String (Price4.to_string p)
let chr c = `String (String.of_char c)

let to_json ~unit ~seq (m : Pitch.Message.t) : Yojson.Safe.t =
  let typ, fields =
    match m with
    | Time { seconds } -> "Time", [ "seconds", `Int seconds ]
    | Add_order { time_offset; order_id; side; quantity; symbol; price = p; flags } ->
      ( "AddOrder"
      , [ "time_offset", `Int time_offset
        ; "order_id", `Int order_id
        ; "side", chr side
        ; "quantity", `Int quantity
        ; "symbol", `String symbol
        ; "price", price p
        ; "flags", `Int flags
        ] )
    | Order_executed { time_offset; order_id; executed_quantity; execution_id; _ } ->
      ( "OrderExecuted"
      , [ "time_offset", `Int time_offset
        ; "order_id", `Int order_id
        ; "executed_quantity", `Int executed_quantity
        ; "execution_id", `String (Boe.exec_id_to_string execution_id)
        ] )
    | Order_executed_at_price
        { time_offset
        ; order_id
        ; executed_quantity
        ; remaining_quantity
        ; execution_id
        ; price = p
        ; _
        } ->
      ( "OrderExecutedAtPrice"
      , [ "time_offset", `Int time_offset
        ; "order_id", `Int order_id
        ; "executed_quantity", `Int executed_quantity
        ; "remaining_quantity", `Int remaining_quantity
        ; "execution_id", `String (Boe.exec_id_to_string execution_id)
        ; "price", price p
        ] )
    | Reduce_size { time_offset; order_id; canceled_quantity } ->
      ( "ReduceSize"
      , [ "time_offset", `Int time_offset
        ; "order_id", `Int order_id
        ; "canceled_quantity", `Int canceled_quantity
        ] )
    | Modify_order { time_offset; order_id; quantity; price = p; flags } ->
      ( "ModifyOrder"
      , [ "time_offset", `Int time_offset
        ; "order_id", `Int order_id
        ; "quantity", `Int quantity
        ; "price", price p
        ; "flags", `Int flags
        ] )
    | Delete_order { time_offset; order_id } ->
      "DeleteOrder", [ "time_offset", `Int time_offset; "order_id", `Int order_id ]
    | Trade { time_offset; order_id; side; quantity; symbol; price = p; execution_id; _ }
      ->
      ( "Trade"
      , [ "time_offset", `Int time_offset
        ; "order_id", `Int order_id
        ; "side", chr side
        ; "quantity", `Int quantity
        ; "symbol", `String symbol
        ; "price", price p
        ; "execution_id", `String (Boe.exec_id_to_string execution_id)
        ] )
    | End_of_session { time_offset } ->
      "EndOfSession", [ "time_offset", `Int time_offset ]
    | Trading_status { time_offset; symbol; status; reg_sho_action } ->
      ( "TradingStatus"
      , [ "time_offset", `Int time_offset
        ; "symbol", `String symbol
        ; "status", chr status
        ; "reg_sho_action", chr reg_sho_action
        ] )
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
      ( "AuctionUpdate"
      , [ "time_offset", `Int time_offset
        ; "symbol", `String symbol
        ; "auction_type", chr auction_type
        ; "reference_price", price reference_price
        ; "buy_shares", `Int buy_shares
        ; "sell_shares", `Int sell_shares
        ; "indicative_price", price indicative_price
        ; "auction_only_price", price auction_only_price
        ] )
    | Auction_summary { time_offset; symbol; auction_type; price = p; shares } ->
      ( "AuctionSummary"
      , [ "time_offset", `Int time_offset
        ; "symbol", `String symbol
        ; "auction_type", chr auction_type
        ; "price", price p
        ; "shares", `Int shares
        ] )
    | Symbol_mapping { feed_symbol; osi_symbol; symbol_condition; underlying } ->
      ( "SymbolMapping"
      , [ "feed_symbol", `String feed_symbol
        ; "osi_symbol", `String osi_symbol
        ; "symbol_condition", chr symbol_condition
        ; "underlying", `String underlying
        ] )
    | Spin_response { sequence; order_count; status } ->
      ( "SpinResponse"
      , [ "sequence", `Int sequence
        ; "order_count", `Int order_count
        ; "status", chr status
        ] )
    | Spin_finished { sequence } -> "SpinFinished", [ "sequence", `Int sequence ]
    | Unknown { message_type; _ } -> "Unknown", [ "message_type", `Int message_type ]
  in
  `Assoc ([ "unit", `Int unit; "seq", `Int seq; "type", `String typ ] @ fields)
;;

let broadcast t ~unit ~seq (m : Pitch.Message.t option) =
  Hashtbl.set t.last_sent ~key:unit ~data:(Time_ns.now ());
  let frame =
    lazy (Pitch.Frame.to_string { unit; sequence = seq; messages = Option.to_list m })
  in
  Hashtbl.iter t.subscribers ~f:(function
    | Binary send -> send (Lazy.force frame)
    | Json send ->
      Option.iter m ~f:(fun m -> send (Yojson.Safe.to_string (to_json ~unit ~seq m))))
;;

let send_sequenced t ~unit m =
  let seq = next_sequence t ~unit in
  Hashtbl.set t.seqs ~key:unit ~data:seq;
  Pitch_book.apply t.book ~unit m;
  broadcast t ~unit ~seq (Some m)
;;

let clock_parts t =
  let _, tod = Clock.local t.clock in
  let ns = Time_ns.Span.to_int_ns tod in
  ns / 1_000_000_000, ns % 1_000_000_000
;;

let time_if_changed t ~unit =
  let seconds, offset = clock_parts t in
  if Option.value_map (Hashtbl.find t.last_time unit) ~default:true ~f:(fun s ->
       s <> seconds)
  then (
    Hashtbl.set t.last_time ~key:unit ~data:seconds;
    send_sequenced t ~unit (Time { seconds }));
  offset
;;

let publish t ~unit m =
  let offset = time_if_changed t ~unit in
  send_sequenced t ~unit (with_offset m offset)
;;

let on_timer t =
  let now = Time_ns.now () in
  List.iter units ~f:(fun unit ->
    ignore (time_if_changed t ~unit : int);
    match Hashtbl.find t.last_sent unit with
    | Some last when Time_ns.Span.(Time_ns.diff now last < second) -> ()
    | _ -> broadcast t ~unit ~seq:(next_sequence t ~unit) None)
;;

let snapshot t ~trading_status ~unit =
  let seconds, _ = clock_parts t in
  let listings =
    List.filter t.instruments ~f:(fun (i : Instruments.t) -> i.unit = unit)
  in
  let mappings =
    List.filter_map listings ~f:(fun (i : Instruments.t) ->
      match i.kind with
      | Equity _ -> None
      | Option { osi; _ } ->
        Some
          (Pitch.Message.Symbol_mapping
             { feed_symbol = i.symbol
             ; osi_symbol = Equities_core.Symbol.Osi.to_string osi
             ; symbol_condition = 'N'
             ; underlying = osi.root
             }))
  in
  let orders = Pitch_book.orders t.book ~unit in
  let sequence = next_sequence t ~unit - 1 in
  [ Pitch.Message.Time { seconds } ]
  @ mappings
  @ List.map listings ~f:trading_status
  @ [ Pitch.Message.Spin_response
        { sequence; order_count = List.length orders; status = 'A' }
    ]
  @ List.map orders ~f:(fun (o : Pitch_book.Order.t) ->
    Pitch.Message.Add_order
      { time_offset = 0
      ; order_id = o.order_id
      ; side = o.side
      ; quantity = o.quantity
      ; symbol = o.symbol
      ; price = o.price
      ; flags = 1
      })
  @ [ Pitch.Message.Spin_finished { sequence } ]
;;

let add t sink =
  let id = t.next_subscriber in
  t.next_subscriber <- id + 1;
  Hashtbl.set t.subscribers ~key:id ~data:sink;
  id
;;

let subscribe t ~trading_status ~send =
  List.iter units ~f:(fun unit ->
    List.iter (snapshot t ~trading_status ~unit) ~f:(fun m ->
      send (Pitch.Frame.to_string { unit; sequence = 0; messages = [ m ] })));
  add t (Binary send)
;;

let subscribe_json t ~trading_status ~send =
  List.iter units ~f:(fun unit ->
    List.iter (snapshot t ~trading_status ~unit) ~f:(fun m ->
      send (Yojson.Safe.to_string (to_json ~unit ~seq:0 m))));
  add t (Json send)
;;

let unsubscribe t id = Hashtbl.remove t.subscribers id
