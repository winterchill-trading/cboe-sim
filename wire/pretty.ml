open! Core

let px = Price4.to_string

let opt name f = function
  | None -> None
  | Some v -> Some (sprintf "%s=%s" name (f v))
;;

let return_fields (r : Boe.Return_fields.t) =
  List.filter_opt
    [ opt "sym" Fn.id r.symbol
    ; opt "side" Char.to_string r.side
    ; opt "px" px r.price
    ; opt "qty" Int.to_string r.order_qty
    ; opt "leaves" Int.to_string r.leaves_qty
    ; opt "orig" Fn.id r.orig_cl_ord_id
    ]
  |> String.concat ~sep:" "
;;

let with_fields s r =
  match return_fields r with
  | "" -> s
  | f -> s ^ " " ^ f
;;

let boe_outbound (m : Boe.Outbound.t) =
  match m with
  | Login_response r ->
    sprintf
      "LoginResponse status=%c text=%S last_seq=%d units=%s"
      r.status
      r.text
      r.last_received_sequence
      (List.map r.units ~f:(fun (u, s) -> sprintf "%d:%d" u s) |> String.concat ~sep:",")
  | Logout r -> sprintf "Logout reason=%c text=%S" r.reason r.text
  | Server_heartbeat -> "ServerHeartbeat"
  | Replay_complete -> "ReplayComplete"
  | Order_acknowledgment { cl_ord_id; order_id; return; _ } ->
    with_fields (sprintf "OrderAck %s order_id=%d" cl_ord_id order_id) return
  | Order_rejected { cl_ord_id; reason; text; _ } ->
    sprintf "OrderRejected %s reason=%c %S" cl_ord_id reason text
  | Order_modified { cl_ord_id; order_id; return; _ } ->
    with_fields (sprintf "OrderModified %s order_id=%d" cl_ord_id order_id) return
  | Order_restated { cl_ord_id; order_id; reason; return; _ } ->
    with_fields
      (sprintf "OrderRestated %s order_id=%d reason=%c" cl_ord_id order_id reason)
      return
  | User_modify_rejected { cl_ord_id; reason; text; _ } ->
    sprintf "ModifyRejected %s reason=%c %S" cl_ord_id reason text
  | Order_cancelled { cl_ord_id; reason; return; _ } ->
    with_fields (sprintf "OrderCancelled %s reason=%c" cl_ord_id reason) return
  | Cancel_rejected { cl_ord_id; reason; text; _ } ->
    sprintf "CancelRejected %s reason=%c %S" cl_ord_id reason text
  | Order_execution
      { cl_ord_id
      ; exec_id
      ; last_shares
      ; last_px
      ; leaves_qty
      ; base_liquidity_indicator
      ; _
      } ->
    sprintf
      "OrderExecution %s exec=%s %d@%s leaves=%d liq=%c"
      cl_ord_id
      (Boe.exec_id_to_string exec_id)
      last_shares
      (px last_px)
      leaves_qty
      base_liquidity_indicator
;;

let boe_inbound (m : Boe.Inbound.t) =
  match m with
  | Login_request r -> sprintf "LoginRequest user=%s sub=%s" r.username r.session_sub_id
  | Logout_request -> "LogoutRequest"
  | Client_heartbeat -> "ClientHeartbeat"
  | New_order n ->
    sprintf
      "NewOrder %s side=%c qty=%d%s%s"
      n.cl_ord_id
      n.side
      n.order_qty
      (Option.value_map n.symbol ~default:"" ~f:(sprintf " sym=%s"))
      (Option.value_map n.price ~default:"" ~f:(fun p -> " px=" ^ px p))
  | Cancel_order c -> sprintf "CancelOrder %s" c.orig_cl_ord_id
  | Modify_order m ->
    sprintf
      "ModifyOrder %s orig=%s%s%s"
      m.cl_ord_id
      m.orig_cl_ord_id
      (Option.value_map m.order_qty ~default:"" ~f:(sprintf " qty=%d"))
      (Option.value_map m.price ~default:"" ~f:(fun p -> " px=" ^ px p))
;;

let pitch (m : Pitch.Message.t) =
  match m with
  | Time { seconds } ->
    sprintf "Time %02d:%02d:%02d" (seconds / 3600) (seconds / 60 % 60) (seconds % 60)
  | Add_order { order_id; side; quantity; symbol; price; _ } ->
    sprintf "AddOrder id=%d %s %c %d@%s" order_id symbol side quantity (px price)
  | Order_executed { order_id; executed_quantity; execution_id; _ } ->
    sprintf
      "OrderExecuted id=%d qty=%d exec=%s"
      order_id
      executed_quantity
      (Boe.exec_id_to_string execution_id)
  | Order_executed_at_price
      { order_id; executed_quantity; remaining_quantity; execution_id; price; _ } ->
    sprintf
      "OrderExecutedAtPrice id=%d qty=%d@%s remaining=%d exec=%s"
      order_id
      executed_quantity
      (px price)
      remaining_quantity
      (Boe.exec_id_to_string execution_id)
  | Reduce_size { order_id; canceled_quantity; _ } ->
    sprintf "ReduceSize id=%d by=%d" order_id canceled_quantity
  | Modify_order { order_id; quantity; price; _ } ->
    sprintf "ModifyOrder id=%d %d@%s" order_id quantity (px price)
  | Delete_order { order_id; _ } -> sprintf "DeleteOrder id=%d" order_id
  | Trade { order_id; quantity; symbol; price; execution_id; _ } ->
    sprintf
      "Trade id=%d %s %d@%s exec=%s"
      order_id
      symbol
      quantity
      (px price)
      (Boe.exec_id_to_string execution_id)
  | End_of_session _ -> "EndOfSession"
  | Trading_status { symbol; status; reg_sho_action; _ } ->
    sprintf "TradingStatus %s %c regsho=%c" symbol status reg_sho_action
  | Auction_update
      { symbol
      ; auction_type
      ; reference_price
      ; buy_shares
      ; sell_shares
      ; indicative_price
      ; auction_only_price
      ; _
      } ->
    sprintf
      "AuctionUpdate %s %c ref=%s buy=%d sell=%d indicative=%s auction_only=%s"
      symbol
      auction_type
      (px reference_price)
      buy_shares
      sell_shares
      (px indicative_price)
      (px auction_only_price)
  | Auction_summary { symbol; auction_type; price; shares; _ } ->
    sprintf "AuctionSummary %s %c %d@%s" symbol auction_type shares (px price)
  | Symbol_mapping { feed_symbol; osi_symbol; underlying; _ } ->
    sprintf "SymbolMapping %s -> %S (%s)" feed_symbol osi_symbol underlying
  | Spin_response { sequence; order_count; status } ->
    sprintf "SpinResponse seq=%d orders=%d status=%c" sequence order_count status
  | Spin_finished { sequence } -> sprintf "SpinFinished seq=%d" sequence
  | Unknown { message_type; payload } ->
    sprintf "Unknown type=0x%02X len=%d" message_type (String.length payload)
;;
