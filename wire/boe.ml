open! Core
module Raw = Bitfields.Raw

module Message_type = struct
  let login_request = 0x37
  let logout_request = 0x02
  let client_heartbeat = 0x03
  let new_order = 0x38
  let cancel_order = 0x39
  let modify_order = 0x3A
  let login_response = 0x24
  let logout = 0x08
  let server_heartbeat = 0x09
  let replay_complete = 0x13
  let order_acknowledgment = 0x25
  let order_rejected = 0x26
  let order_modified = 0x27
  let order_restated = 0x28
  let user_modify_rejected = 0x29
  let order_cancelled = 0x2A
  let cancel_rejected = 0x2B
  let order_execution = 0x2C

  let with_return_bitfields =
    [ order_acknowledgment
    ; order_rejected
    ; order_modified
    ; order_restated
    ; user_modify_rejected
    ; order_cancelled
    ; cancel_rejected
    ; order_execution
    ]
  ;;

  let names =
    [ login_request, "LoginRequest"
    ; logout_request, "LogoutRequest"
    ; client_heartbeat, "ClientHeartbeat"
    ; new_order, "NewOrder"
    ; cancel_order, "CancelOrder"
    ; modify_order, "ModifyOrder"
    ; login_response, "LoginResponse"
    ; logout, "Logout"
    ; server_heartbeat, "ServerHeartbeat"
    ; replay_complete, "ReplayComplete"
    ; order_acknowledgment, "OrderAcknowledgment"
    ; order_rejected, "OrderRejected"
    ; order_modified, "OrderModified"
    ; order_restated, "OrderRestated"
    ; user_modify_rejected, "UserModifyRejected"
    ; order_cancelled, "OrderCancelled"
    ; cancel_rejected, "CancelRejected"
    ; order_execution, "OrderExecution"
    ]
  ;;

  let name t =
    match List.Assoc.find names t ~equal:Int.equal with
    | Some n -> n
    | None -> sprintf "0x%02X" t
  ;;
end

let header_length = 10

let frame_length buf ~pos =
  if Le.get_u8 buf ~pos <> 0xBA || Le.get_u8 buf ~pos:(pos + 1) <> 0xBA
  then Or_error.error_string "BOE frame does not start with 0xBABA"
  else Ok (Le.get_u16 buf ~pos:(pos + 2) + 2)
;;

module Unit_sequences = struct
  type t =
    { no_unspecified_unit_replay : bool
    ; units : (int * int) list
    }
  [@@deriving sexp, compare, equal]
end

module Login_request = struct
  type t =
    { session_sub_id : string
    ; username : string
    ; password : string
    ; unit_sequences : Unit_sequences.t option
    ; return_bitfields : (int * string) list
    }
  [@@deriving sexp, compare, equal]
end

module Login_response = struct
  type t =
    { status : char
    ; text : string
    ; no_unspecified_unit_replay : bool
    ; last_received_sequence : int
    ; units : (int * int) list
    ; return_bitfields : (int * string) list
    }
  [@@deriving sexp, compare, equal]
end

module Logout = struct
  type t =
    { reason : char
    ; text : string
    ; last_received_sequence : int
    ; units : (int * int) list
    }
  [@@deriving sexp, compare, equal]
end

let get name l ~f = List.Assoc.find l name ~equal:String.equal |> Option.map ~f

let opt name v ~f =
  match v with
  | None -> None
  | Some v -> Some (name, f v)
;;

module New_order = struct
  type t =
    { cl_ord_id : string
    ; side : char
    ; order_qty : int
    ; clearing_firm : string option
    ; price : Price4.t option
    ; exec_inst : char option
    ; ord_type : char option
    ; time_in_force : char option
    ; min_qty : int option
    ; max_floor : int option
    ; symbol : string option
    ; capacity : char option
    ; routing_inst : string option
    ; account : string option
    ; display_indicator : char option
    ; prevent_match : string option
    ; maturity_date : int option
    ; strike_price : Price4.t option
    ; put_or_call : char option
    ; open_close : char option
    }
  [@@deriving sexp, compare, equal]

  let create ~cl_ord_id ~side ~order_qty ~symbol ?(capacity = 'A') () =
    { cl_ord_id
    ; side
    ; order_qty
    ; clearing_firm = None
    ; price = None
    ; exec_inst = None
    ; ord_type = None
    ; time_in_force = None
    ; min_qty = None
    ; max_floor = None
    ; symbol = Some symbol
    ; capacity = Some capacity
    ; routing_inst = None
    ; account = None
    ; display_indicator = None
    ; prevent_match = None
    ; maturity_date = None
    ; strike_price = None
    ; put_or_call = None
    ; open_close = None
    }
  ;;

  let to_assoc t =
    List.filter_opt
      [ opt "ClearingFirm" t.clearing_firm ~f:(Raw.text ~len:4)
      ; opt "Price" t.price ~f:Raw.i64
      ; opt "ExecInst" t.exec_inst ~f:Raw.char
      ; opt "OrdType" t.ord_type ~f:Raw.char
      ; opt "TimeInForce" t.time_in_force ~f:Raw.char
      ; opt "MinQty" t.min_qty ~f:Raw.u32
      ; opt "MaxFloor" t.max_floor ~f:Raw.u32
      ; opt "Symbol" t.symbol ~f:(Raw.text ~len:8)
      ; opt "Capacity" t.capacity ~f:Raw.char
      ; opt "RoutingInst" t.routing_inst ~f:(Raw.text ~len:4)
      ; opt "Account" t.account ~f:(Raw.text ~len:16)
      ; opt "DisplayIndicator" t.display_indicator ~f:Raw.char
      ; opt "PreventMatch" t.prevent_match ~f:(Raw.text ~len:3)
      ; opt "MaturityDate" t.maturity_date ~f:Raw.u32
      ; opt "StrikePrice" t.strike_price ~f:Raw.i64
      ; opt "PutOrCall" t.put_or_call ~f:Raw.char
      ; opt "OpenClose" t.open_close ~f:Raw.char
      ]
  ;;

  let of_assoc ~cl_ord_id ~side ~order_qty l =
    { cl_ord_id
    ; side
    ; order_qty
    ; clearing_firm = get "ClearingFirm" l ~f:Raw.to_text
    ; price = get "Price" l ~f:Raw.to_int
    ; exec_inst = get "ExecInst" l ~f:Raw.to_char
    ; ord_type = get "OrdType" l ~f:Raw.to_char
    ; time_in_force = get "TimeInForce" l ~f:Raw.to_char
    ; min_qty = get "MinQty" l ~f:Raw.to_int
    ; max_floor = get "MaxFloor" l ~f:Raw.to_int
    ; symbol = get "Symbol" l ~f:Raw.to_text
    ; capacity = get "Capacity" l ~f:Raw.to_char
    ; routing_inst = get "RoutingInst" l ~f:Raw.to_text
    ; account = get "Account" l ~f:Raw.to_text
    ; display_indicator = get "DisplayIndicator" l ~f:Raw.to_char
    ; prevent_match = get "PreventMatch" l ~f:Raw.to_text
    ; maturity_date = get "MaturityDate" l ~f:Raw.to_int
    ; strike_price = get "StrikePrice" l ~f:Raw.to_int
    ; put_or_call = get "PutOrCall" l ~f:Raw.to_char
    ; open_close = get "OpenClose" l ~f:Raw.to_char
    }
  ;;
end

module Cancel_order = struct
  type t = { orig_cl_ord_id : string } [@@deriving sexp, compare, equal]
end

module Modify_order = struct
  type t =
    { cl_ord_id : string
    ; orig_cl_ord_id : string
    ; order_qty : int option
    ; price : Price4.t option
    ; ord_type : char option
    ; cancel_orig_on_reject : char option
    ; side : char option
    ; max_floor : int option
    }
  [@@deriving sexp, compare, equal]

  let to_assoc t =
    List.filter_opt
      [ opt "OrderQty" t.order_qty ~f:Raw.u32
      ; opt "Price" t.price ~f:Raw.i64
      ; opt "OrdType" t.ord_type ~f:Raw.char
      ; opt "CancelOrigOnReject" t.cancel_orig_on_reject ~f:Raw.char
      ; opt "Side" t.side ~f:Raw.char
      ; opt "MaxFloor" t.max_floor ~f:Raw.u32
      ]
  ;;

  let of_assoc ~cl_ord_id ~orig_cl_ord_id l =
    { cl_ord_id
    ; orig_cl_ord_id
    ; order_qty = get "OrderQty" l ~f:Raw.to_int
    ; price = get "Price" l ~f:Raw.to_int
    ; ord_type = get "OrdType" l ~f:Raw.to_char
    ; cancel_orig_on_reject = get "CancelOrigOnReject" l ~f:Raw.to_char
    ; side = get "Side" l ~f:Raw.to_char
    ; max_floor = get "MaxFloor" l ~f:Raw.to_int
    }
  ;;
end

module Return_fields = struct
  type t =
    { side : char option
    ; price : Price4.t option
    ; exec_inst : char option
    ; ord_type : char option
    ; time_in_force : char option
    ; min_qty : int option
    ; symbol : string option
    ; capacity : char option
    ; account : string option
    ; clearing_firm : string option
    ; display_indicator : char option
    ; max_floor : int option
    ; order_qty : int option
    ; prevent_match : string option
    ; maturity_date : int option
    ; strike_price : Price4.t option
    ; put_or_call : char option
    ; open_close : char option
    ; orig_cl_ord_id : string option
    ; leaves_qty : int option
    ; last_shares : int option
    ; last_px : Price4.t option
    ; display_price : Price4.t option
    ; working_price : Price4.t option
    ; base_liquidity_indicator : char option
    }
  [@@deriving sexp, compare, equal]

  let empty =
    { side = None
    ; price = None
    ; exec_inst = None
    ; ord_type = None
    ; time_in_force = None
    ; min_qty = None
    ; symbol = None
    ; capacity = None
    ; account = None
    ; clearing_firm = None
    ; display_indicator = None
    ; max_floor = None
    ; order_qty = None
    ; prevent_match = None
    ; maturity_date = None
    ; strike_price = None
    ; put_or_call = None
    ; open_close = None
    ; orig_cl_ord_id = None
    ; leaves_qty = None
    ; last_shares = None
    ; last_px = None
    ; display_price = None
    ; working_price = None
    ; base_liquidity_indicator = None
    }
  ;;

  let to_assoc t =
    List.filter_opt
      [ opt "Side" t.side ~f:Raw.char
      ; opt "Price" t.price ~f:Raw.i64
      ; opt "ExecInst" t.exec_inst ~f:Raw.char
      ; opt "OrdType" t.ord_type ~f:Raw.char
      ; opt "TimeInForce" t.time_in_force ~f:Raw.char
      ; opt "MinQty" t.min_qty ~f:Raw.u32
      ; opt "Symbol" t.symbol ~f:(Raw.text ~len:8)
      ; opt "Capacity" t.capacity ~f:Raw.char
      ; opt "Account" t.account ~f:(Raw.text ~len:16)
      ; opt "ClearingFirm" t.clearing_firm ~f:(Raw.text ~len:4)
      ; opt "DisplayIndicator" t.display_indicator ~f:Raw.char
      ; opt "MaxFloor" t.max_floor ~f:Raw.u32
      ; opt "OrderQty" t.order_qty ~f:Raw.u32
      ; opt "PreventMatch" t.prevent_match ~f:(Raw.text ~len:3)
      ; opt "MaturityDate" t.maturity_date ~f:Raw.u32
      ; opt "StrikePrice" t.strike_price ~f:Raw.i64
      ; opt "PutOrCall" t.put_or_call ~f:Raw.char
      ; opt "OpenClose" t.open_close ~f:Raw.char
      ; opt "OrigClOrdID" t.orig_cl_ord_id ~f:(Raw.text ~len:20)
      ; opt "LeavesQty" t.leaves_qty ~f:Raw.u32
      ; opt "LastShares" t.last_shares ~f:Raw.u32
      ; opt "LastPx" t.last_px ~f:Raw.i64
      ; opt "DisplayPrice" t.display_price ~f:Raw.i64
      ; opt "WorkingPrice" t.working_price ~f:Raw.i64
      ; opt "BaseLiquidityIndicator" t.base_liquidity_indicator ~f:Raw.char
      ]
  ;;

  let of_assoc l =
    { side = get "Side" l ~f:Raw.to_char
    ; price = get "Price" l ~f:Raw.to_int
    ; exec_inst = get "ExecInst" l ~f:Raw.to_char
    ; ord_type = get "OrdType" l ~f:Raw.to_char
    ; time_in_force = get "TimeInForce" l ~f:Raw.to_char
    ; min_qty = get "MinQty" l ~f:Raw.to_int
    ; symbol = get "Symbol" l ~f:Raw.to_text
    ; capacity = get "Capacity" l ~f:Raw.to_char
    ; account = get "Account" l ~f:Raw.to_text
    ; clearing_firm = get "ClearingFirm" l ~f:Raw.to_text
    ; display_indicator = get "DisplayIndicator" l ~f:Raw.to_char
    ; max_floor = get "MaxFloor" l ~f:Raw.to_int
    ; order_qty = get "OrderQty" l ~f:Raw.to_int
    ; prevent_match = get "PreventMatch" l ~f:Raw.to_text
    ; maturity_date = get "MaturityDate" l ~f:Raw.to_int
    ; strike_price = get "StrikePrice" l ~f:Raw.to_int
    ; put_or_call = get "PutOrCall" l ~f:Raw.to_char
    ; open_close = get "OpenClose" l ~f:Raw.to_char
    ; orig_cl_ord_id = get "OrigClOrdID" l ~f:Raw.to_text
    ; leaves_qty = get "LeavesQty" l ~f:Raw.to_int
    ; last_shares = get "LastShares" l ~f:Raw.to_int
    ; last_px = get "LastPx" l ~f:Raw.to_int
    ; display_price = get "DisplayPrice" l ~f:Raw.to_int
    ; working_price = get "WorkingPrice" l ~f:Raw.to_int
    ; base_liquidity_indicator = get "BaseLiquidityIndicator" l ~f:Raw.to_char
    }
  ;;
end

(* Encoding helpers *)

let frame ~typ ~matching_unit ~sequence ~body_len fill =
  let total = header_length + body_len in
  let b = Bigstring.create total in
  Bigstring.memset b ~pos:0 ~len:total '\000';
  Le.set_u8 b ~pos:0 0xBA;
  Le.set_u8 b ~pos:1 0xBA;
  Le.set_u16 b ~pos:2 (total - 2);
  Le.set_u8 b ~pos:4 typ;
  Le.set_u8 b ~pos:5 matching_unit;
  Le.set_u32 b ~pos:6 sequence;
  fill b;
  Bigstring.to_string b
;;

let text b ~pos ~len s = Le.set_text b ~pad:Nul ~pos ~len s
let read_text b ~pos ~len = Le.get_text b ~pad:Nul ~pos ~len
let units_size units = 1 + (5 * List.length units)

let write_units b ~pos units =
  Le.set_u8 b ~pos (List.length units);
  List.iteri units ~f:(fun i (u, s) ->
    Le.set_u8 b ~pos:(pos + 1 + (5 * i)) u;
    Le.set_u32 b ~pos:(pos + 2 + (5 * i)) s)
;;

let read_units b ~pos ~limit =
  let n = Le.get_u8 b ~pos in
  if pos + 1 + (5 * n) > limit
  then Or_error.error_string "truncated unit list"
  else
    Ok
      ( List.init n ~f:(fun i ->
          Le.get_u8 b ~pos:(pos + 1 + (5 * i)), Le.get_u32 b ~pos:(pos + 2 + (5 * i)))
      , pos + 1 + (5 * n) )
;;

(* Login parameter groups: 0x80 unit sequences, 0x81 return bitfields. *)

let group_size_units (u : Unit_sequences.t) = 4 + units_size u.units
let group_size_bitfields bf = 5 + String.length bf

let write_groups b ~pos ~unit_sequences ~return_bitfields =
  let n = Option.length unit_sequences + List.length return_bitfields in
  Le.set_u8 b ~pos n;
  let p = ref (pos + 1) in
  Option.iter unit_sequences ~f:(fun (u : Unit_sequences.t) ->
    let len = group_size_units u in
    Le.set_u16 b ~pos:!p len;
    Le.set_u8 b ~pos:(!p + 2) 0x80;
    Le.set_u8 b ~pos:(!p + 3) (Bool.to_int u.no_unspecified_unit_replay);
    write_units b ~pos:(!p + 4) u.units;
    p := !p + len);
  List.iter return_bitfields ~f:(fun (typ, bf) ->
    let len = group_size_bitfields bf in
    Le.set_u16 b ~pos:!p len;
    Le.set_u8 b ~pos:(!p + 2) 0x81;
    Le.set_u8 b ~pos:(!p + 3) typ;
    Le.set_u8 b ~pos:(!p + 4) (String.length bf);
    String.iteri bf ~f:(fun i c -> Le.set_char b ~pos:(!p + 5 + i) c);
    p := !p + len)
;;

let groups_size ~unit_sequences ~return_bitfields =
  1
  + Option.value_map unit_sequences ~default:0 ~f:group_size_units
  + List.sum (module Int) return_bitfields ~f:(fun (_, bf) -> group_size_bitfields bf)
;;

let read_groups b ~pos ~limit =
  let open Or_error.Let_syntax in
  if pos >= limit
  then Ok (None, [])
  else (
    let n = Le.get_u8 b ~pos in
    let rec loop i p units bitfields =
      if i = n
      then Ok (units, List.rev bitfields)
      else if p + 3 > limit
      then Or_error.error_string "truncated parameter group"
      else (
        let len = Le.get_u16 b ~pos:p in
        let%bind () =
          if len >= 3 && p + len <= limit
          then Ok ()
          else Or_error.error_string "bad parameter group length"
        in
        match Le.get_u8 b ~pos:(p + 2) with
        | 0x80 ->
          let%bind us, _ = read_units b ~pos:(p + 4) ~limit:(p + len) in
          let u =
            { Unit_sequences.no_unspecified_unit_replay = Le.get_u8 b ~pos:(p + 3) <> 0
            ; units = us
            }
          in
          loop (i + 1) (p + len) (Some u) bitfields
        | 0x81 ->
          let typ = Le.get_u8 b ~pos:(p + 3) in
          let m = Le.get_u8 b ~pos:(p + 4) in
          let%bind () =
            if 5 + m <= len then Ok () else Or_error.error_string "bad bitfield group"
          in
          let bf = Bigstring.To_string.sub b ~pos:(p + 5) ~len:m in
          loop (i + 1) (p + len) units ((typ, bf) :: bitfields)
        | other -> Or_error.error_s [%message "unknown parameter group" (other : int)])
    in
    loop 0 (pos + 1) None [])
;;

let check_length ~len ~min name =
  if len >= min
  then Ok ()
  else Or_error.error_s [%message "message too short" (name : string) (len : int)]
;;

let check_frame b ~pos ~len =
  let open Or_error.Let_syntax in
  let%bind () = check_length ~len ~min:header_length "header" in
  let%bind total = frame_length b ~pos in
  if total <> len
  then Or_error.error_s [%message "frame length mismatch" (total : int) (len : int)]
  else
    Ok
      (Le.get_u8 b ~pos:(pos + 4), Le.get_u8 b ~pos:(pos + 5), Le.get_u32 b ~pos:(pos + 6))
;;

module Inbound = struct
  type t =
    | Login_request of Login_request.t
    | Logout_request
    | Client_heartbeat
    | New_order of New_order.t
    | Cancel_order of Cancel_order.t
    | Modify_order of Modify_order.t
  [@@deriving sexp, compare, equal]

  let to_string ~sequence t =
    let session typ body_len fill =
      frame ~typ ~matching_unit:0 ~sequence:0 ~body_len fill
    in
    let app typ body_len fill = frame ~typ ~matching_unit:0 ~sequence ~body_len fill in
    match t with
    | Logout_request -> session Message_type.logout_request 0 ignore
    | Client_heartbeat -> session Message_type.client_heartbeat 0 ignore
    | Login_request r ->
      let groups =
        groups_size ~unit_sequences:r.unit_sequences ~return_bitfields:r.return_bitfields
      in
      session Message_type.login_request (18 + groups) (fun b ->
        text b ~pos:10 ~len:4 r.session_sub_id;
        text b ~pos:14 ~len:4 r.username;
        text b ~pos:18 ~len:10 r.password;
        write_groups
          b
          ~pos:28
          ~unit_sequences:r.unit_sequences
          ~return_bitfields:r.return_bitfields)
    | New_order o ->
      let assoc = New_order.to_assoc o in
      let table = Bitfields.new_order in
      let bitfields = Bitfields.bitfields_of_names table (List.map assoc ~f:fst) in
      let opt = Bitfields.size table ~bitfields in
      app Message_type.new_order (25 + opt) (fun b ->
        text b ~pos:10 ~len:20 o.cl_ord_id;
        Le.set_char b ~pos:30 o.side;
        Le.set_u32 b ~pos:31 o.order_qty;
        ignore
          (Bitfields.write table b ~pos:35 ~bitfields ~lookup:(fun name ->
             List.Assoc.find assoc name ~equal:String.equal)
           : int))
    | Cancel_order c ->
      app Message_type.cancel_order 21 (fun b -> text b ~pos:10 ~len:20 c.orig_cl_ord_id)
    | Modify_order m ->
      let assoc = Modify_order.to_assoc m in
      let table = Bitfields.modify_order in
      let bitfields = Bitfields.bitfields_of_names table (List.map assoc ~f:fst) in
      let opt = Bitfields.size table ~bitfields in
      app Message_type.modify_order (40 + opt) (fun b ->
        text b ~pos:10 ~len:20 m.cl_ord_id;
        text b ~pos:30 ~len:20 m.orig_cl_ord_id;
        ignore
          (Bitfields.write table b ~pos:50 ~bitfields ~lookup:(fun name ->
             List.Assoc.find assoc name ~equal:String.equal)
           : int))
  ;;

  let of_bigstring b ~pos ~len =
    let open Or_error.Let_syntax in
    let%bind typ, _unit, sequence = check_frame b ~pos ~len in
    let limit = pos + len in
    let at o = pos + o in
    let%map msg =
      if typ = Message_type.logout_request
      then Ok Logout_request
      else if typ = Message_type.client_heartbeat
      then Ok Client_heartbeat
      else if typ = Message_type.login_request
      then (
        let%bind () = check_length ~len ~min:29 "LoginRequest" in
        let%map unit_sequences, return_bitfields = read_groups b ~pos:(at 28) ~limit in
        Login_request
          { session_sub_id = read_text b ~pos:(at 10) ~len:4
          ; username = read_text b ~pos:(at 14) ~len:4
          ; password = read_text b ~pos:(at 18) ~len:10
          ; unit_sequences
          ; return_bitfields
          })
      else if typ = Message_type.new_order
      then (
        let%bind () = check_length ~len ~min:36 "NewOrder" in
        let%map _, fields =
          Bitfields.read Bitfields.new_order b ~pos:(at 35) ~len:(len - 35)
        in
        New_order
          (New_order.of_assoc
             ~cl_ord_id:(read_text b ~pos:(at 10) ~len:20)
             ~side:(Le.get_char b ~pos:(at 30))
             ~order_qty:(Le.get_u32 b ~pos:(at 31))
             fields))
      else if typ = Message_type.cancel_order
      then (
        let%bind () = check_length ~len ~min:31 "CancelOrder" in
        let%map _ =
          Bitfields.read Bitfields.cancel_order b ~pos:(at 30) ~len:(len - 30)
        in
        Cancel_order { orig_cl_ord_id = read_text b ~pos:(at 10) ~len:20 })
      else if typ = Message_type.modify_order
      then (
        let%bind () = check_length ~len ~min:51 "ModifyOrder" in
        let%map _, fields =
          Bitfields.read Bitfields.modify_order b ~pos:(at 50) ~len:(len - 50)
        in
        Modify_order
          (Modify_order.of_assoc
             ~cl_ord_id:(read_text b ~pos:(at 10) ~len:20)
             ~orig_cl_ord_id:(read_text b ~pos:(at 30) ~len:20)
             fields))
      else Or_error.error_s [%message "unknown inbound message type" (typ : int)]
    in
    sequence, msg
  ;;

  let of_string s = of_bigstring (Bigstring.of_string s) ~pos:0 ~len:(String.length s)
end

module Outbound = struct
  type t =
    | Login_response of Login_response.t
    | Logout of Logout.t
    | Server_heartbeat
    | Replay_complete
    | Order_acknowledgment of
        { transaction_time : int
        ; cl_ord_id : string
        ; order_id : int
        ; return : Return_fields.t
        }
    | Order_rejected of
        { transaction_time : int
        ; cl_ord_id : string
        ; reason : char
        ; text : string
        ; return : Return_fields.t
        }
    | Order_modified of
        { transaction_time : int
        ; cl_ord_id : string
        ; order_id : int
        ; return : Return_fields.t
        }
    | Order_restated of
        { transaction_time : int
        ; cl_ord_id : string
        ; order_id : int
        ; reason : char
        ; return : Return_fields.t
        }
    | User_modify_rejected of
        { transaction_time : int
        ; cl_ord_id : string
        ; reason : char
        ; text : string
        ; return : Return_fields.t
        }
    | Order_cancelled of
        { transaction_time : int
        ; cl_ord_id : string
        ; reason : char
        ; return : Return_fields.t
        }
    | Cancel_rejected of
        { transaction_time : int
        ; cl_ord_id : string
        ; reason : char
        ; text : string
        ; return : Return_fields.t
        }
    | Order_execution of
        { transaction_time : int
        ; cl_ord_id : string
        ; exec_id : int
        ; last_shares : int
        ; last_px : Price4.t
        ; leaves_qty : int
        ; base_liquidity_indicator : char
        ; sub_liquidity_indicator : char
        ; contra_broker : string
        ; return : Return_fields.t
        }
  [@@deriving sexp, compare, equal]

  let message_type = function
    | Login_response _ -> Message_type.login_response
    | Logout _ -> Message_type.logout
    | Server_heartbeat -> Message_type.server_heartbeat
    | Replay_complete -> Message_type.replay_complete
    | Order_acknowledgment _ -> Message_type.order_acknowledgment
    | Order_rejected _ -> Message_type.order_rejected
    | Order_modified _ -> Message_type.order_modified
    | Order_restated _ -> Message_type.order_restated
    | User_modify_rejected _ -> Message_type.user_modify_rejected
    | Order_cancelled _ -> Message_type.order_cancelled
    | Cancel_rejected _ -> Message_type.cancel_rejected
    | Order_execution _ -> Message_type.order_execution
  ;;

  let is_sequenced = function
    | Order_acknowledgment _
    | Order_modified _
    | Order_restated _
    | Order_cancelled _
    | Order_execution _ -> true
    | Login_response _
    | Logout _
    | Server_heartbeat
    | Replay_complete
    | Order_rejected _
    | User_modify_rejected _
    | Cancel_rejected _ -> false
  ;;

  let to_string ~matching_unit ~sequence ~return_bitfields t =
    let typ = message_type t in
    let matching_unit, sequence =
      if is_sequenced t then matching_unit, sequence else 0, 0
    in
    let session body_len fill = frame ~typ ~matching_unit ~sequence ~body_len fill in
    (* Application messages: fixed part up to [count_at], then the return bitfields. *)
    let app ~count_at (return : Return_fields.t) fill =
      let bitfields = return_bitfields typ in
      let table = Bitfields.return in
      let assoc = Return_fields.to_assoc return in
      let opt = Bitfields.size table ~bitfields in
      frame
        ~typ
        ~matching_unit
        ~sequence
        ~body_len:(count_at - header_length + opt)
        (fun b ->
          fill b;
          ignore
            (Bitfields.write table b ~pos:count_at ~bitfields ~lookup:(fun name ->
               List.Assoc.find assoc name ~equal:String.equal)
             : int))
    in
    let time_and_id b ~transaction_time ~cl_ord_id =
      Le.set_i64 b ~pos:10 transaction_time;
      text b ~pos:18 ~len:20 cl_ord_id
    in
    match t with
    | Server_heartbeat | Replay_complete -> session 0 ignore
    | Login_response r ->
      let groups =
        groups_size ~unit_sequences:None ~return_bitfields:r.return_bitfields
      in
      session
        (67 + units_size r.units + groups)
        (fun b ->
          Le.set_char b ~pos:10 r.status;
          text b ~pos:11 ~len:60 r.text;
          Le.set_u8 b ~pos:71 (Bool.to_int r.no_unspecified_unit_replay);
          Le.set_u32 b ~pos:72 r.last_received_sequence;
          write_units b ~pos:76 r.units;
          write_groups
            b
            ~pos:(76 + units_size r.units)
            ~unit_sequences:None
            ~return_bitfields:r.return_bitfields)
    | Logout r ->
      session
        (65 + units_size r.units)
        (fun b ->
          Le.set_char b ~pos:10 r.reason;
          text b ~pos:11 ~len:60 r.text;
          Le.set_u32 b ~pos:71 r.last_received_sequence;
          write_units b ~pos:75 r.units)
    | Order_acknowledgment { transaction_time; cl_ord_id; order_id; return } ->
      app ~count_at:47 return (fun b ->
        time_and_id b ~transaction_time ~cl_ord_id;
        Le.set_u64 b ~pos:38 order_id)
    | Order_modified { transaction_time; cl_ord_id; order_id; return } ->
      app ~count_at:47 return (fun b ->
        time_and_id b ~transaction_time ~cl_ord_id;
        Le.set_u64 b ~pos:38 order_id)
    | Order_restated { transaction_time; cl_ord_id; order_id; reason; return } ->
      app ~count_at:48 return (fun b ->
        time_and_id b ~transaction_time ~cl_ord_id;
        Le.set_u64 b ~pos:38 order_id;
        Le.set_char b ~pos:46 reason)
    | Order_rejected { transaction_time; cl_ord_id; reason; text = msg; return }
    | User_modify_rejected { transaction_time; cl_ord_id; reason; text = msg; return }
    | Cancel_rejected { transaction_time; cl_ord_id; reason; text = msg; return } ->
      app ~count_at:100 return (fun b ->
        time_and_id b ~transaction_time ~cl_ord_id;
        Le.set_char b ~pos:38 reason;
        text b ~pos:39 ~len:60 msg)
    | Order_cancelled { transaction_time; cl_ord_id; reason; return } ->
      app ~count_at:40 return (fun b ->
        time_and_id b ~transaction_time ~cl_ord_id;
        Le.set_char b ~pos:38 reason)
    | Order_execution
        { transaction_time
        ; cl_ord_id
        ; exec_id
        ; last_shares
        ; last_px
        ; leaves_qty
        ; base_liquidity_indicator
        ; sub_liquidity_indicator
        ; contra_broker
        ; return
        } ->
      app ~count_at:69 return (fun b ->
        time_and_id b ~transaction_time ~cl_ord_id;
        Le.set_u64 b ~pos:38 exec_id;
        Le.set_u32 b ~pos:46 last_shares;
        Le.set_i64 b ~pos:50 last_px;
        Le.set_u32 b ~pos:58 leaves_qty;
        Le.set_char b ~pos:62 base_liquidity_indicator;
        Le.set_char b ~pos:63 sub_liquidity_indicator;
        text b ~pos:64 ~len:4 contra_broker)
  ;;

  let of_bigstring b ~pos ~len =
    let open Or_error.Let_syntax in
    let%bind typ, matching_unit, sequence = check_frame b ~pos ~len in
    let limit = pos + len in
    let at o = pos + o in
    let return ~count_at =
      let%bind () = check_length ~len ~min:(count_at + 1) (Message_type.name typ) in
      let%map _, fields =
        Bitfields.read Bitfields.return b ~pos:(at count_at) ~len:(len - count_at)
      in
      Return_fields.of_assoc fields
    in
    let transaction_time () = Le.get_i64 b ~pos:(at 10) in
    let cl_ord_id () = read_text b ~pos:(at 18) ~len:20 in
    let%map msg =
      if typ = Message_type.server_heartbeat
      then Ok Server_heartbeat
      else if typ = Message_type.replay_complete
      then Ok Replay_complete
      else if typ = Message_type.login_response
      then (
        let%bind () = check_length ~len ~min:77 "LoginResponse" in
        let%bind units, p = read_units b ~pos:(at 76) ~limit in
        let%map _, return_bitfields = read_groups b ~pos:p ~limit in
        Login_response
          { status = Le.get_char b ~pos:(at 10)
          ; text = read_text b ~pos:(at 11) ~len:60
          ; no_unspecified_unit_replay = Le.get_u8 b ~pos:(at 71) <> 0
          ; last_received_sequence = Le.get_u32 b ~pos:(at 72)
          ; units
          ; return_bitfields
          })
      else if typ = Message_type.logout
      then (
        let%bind () = check_length ~len ~min:76 "Logout" in
        let%map units, _ = read_units b ~pos:(at 75) ~limit in
        Logout
          { reason = Le.get_char b ~pos:(at 10)
          ; text = read_text b ~pos:(at 11) ~len:60
          ; last_received_sequence = Le.get_u32 b ~pos:(at 71)
          ; units
          })
      else if typ = Message_type.order_acknowledgment
      then (
        let%map return = return ~count_at:47 in
        Order_acknowledgment
          { transaction_time = transaction_time ()
          ; cl_ord_id = cl_ord_id ()
          ; order_id = Le.get_u64 b ~pos:(at 38)
          ; return
          })
      else if typ = Message_type.order_modified
      then (
        let%map return = return ~count_at:47 in
        Order_modified
          { transaction_time = transaction_time ()
          ; cl_ord_id = cl_ord_id ()
          ; order_id = Le.get_u64 b ~pos:(at 38)
          ; return
          })
      else if typ = Message_type.order_restated
      then (
        let%map return = return ~count_at:48 in
        Order_restated
          { transaction_time = transaction_time ()
          ; cl_ord_id = cl_ord_id ()
          ; order_id = Le.get_u64 b ~pos:(at 38)
          ; reason = Le.get_char b ~pos:(at 46)
          ; return
          })
      else if typ = Message_type.order_rejected
              || typ = Message_type.user_modify_rejected
              || typ = Message_type.cancel_rejected
      then (
        let%map return = return ~count_at:100 in
        let transaction_time = transaction_time ()
        and cl_ord_id = cl_ord_id ()
        and reason = Le.get_char b ~pos:(at 38)
        and text = read_text b ~pos:(at 39) ~len:60 in
        if typ = Message_type.order_rejected
        then Order_rejected { transaction_time; cl_ord_id; reason; text; return }
        else if typ = Message_type.user_modify_rejected
        then User_modify_rejected { transaction_time; cl_ord_id; reason; text; return }
        else Cancel_rejected { transaction_time; cl_ord_id; reason; text; return })
      else if typ = Message_type.order_cancelled
      then (
        let%map return = return ~count_at:40 in
        Order_cancelled
          { transaction_time = transaction_time ()
          ; cl_ord_id = cl_ord_id ()
          ; reason = Le.get_char b ~pos:(at 38)
          ; return
          })
      else if typ = Message_type.order_execution
      then (
        let%map return = return ~count_at:69 in
        Order_execution
          { transaction_time = transaction_time ()
          ; cl_ord_id = cl_ord_id ()
          ; exec_id = Le.get_u64 b ~pos:(at 38)
          ; last_shares = Le.get_u32 b ~pos:(at 46)
          ; last_px = Le.get_i64 b ~pos:(at 50)
          ; leaves_qty = Le.get_u32 b ~pos:(at 58)
          ; base_liquidity_indicator = Le.get_char b ~pos:(at 62)
          ; sub_liquidity_indicator = Le.get_char b ~pos:(at 63)
          ; contra_broker = read_text b ~pos:(at 64) ~len:4
          ; return
          })
      else Or_error.error_s [%message "unknown outbound message type" (typ : int)]
    in
    matching_unit, sequence, msg
  ;;

  let of_string s = of_bigstring (Bigstring.of_string s) ~pos:0 ~len:(String.length s)
end

let exec_id_to_string id =
  let digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" in
  String.init 9 ~f:(fun i ->
    let rec pow n = if n = 0 then 1 else 36 * pow (n - 1) in
    digits.[id / pow (8 - i) % 36])
;;
