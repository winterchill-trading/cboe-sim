open! Core
open Cboe_wire
open Equities_core
module E = Exchange_core
module Exchange = Exchange_core_engine.Exchange
module Event = Exchange_core_engine.Event
module Market_sim = Exchange_core_engine.Market_sim
module Price = E.Price
module Size = E.Size
module Order_id = E.Order_id
module Phase = Session.Us_equities.Phase

module Config = struct
  type t =
    { noise : bool
    ; seed : int
    ; noise_traders : int
    ; auction_lead : Time_ns.Span.t
    }

  let default =
    { noise = true; seed = 0; noise_traders = 6; auction_lead = Time_ns.Span.of_min 5. }
  ;;
end

type tif =
  | Day
  | Ioc
  | Fok
  | Opg
  | Clo
[@@deriving equal]

type order =
  { user : string
  ; account : E.Account.t
  ; inst : Instruments.t
  ; esym : E.Symbol.t
  ; side : Order_types.Side.t
  ; side_char : char
  ; mutable cl_ord_id : string
  ; mutable orig_cl_ord_id : string option
  ; mutable venue_id : int
  ; mutable engine_oid : Order_id.t option
  ; mutable price : Price.t option
  ; mutable order_qty : int
  ; mutable cum_qty : int
  ; ord_type : char
  ; tif_char : char
  ; tif : tif
  ; capacity : char
  ; max_floor : int
  ; hidden : bool
  ; post_only : bool
  ; prevent_match : string option
  ; exec_inst : char option
  ; mutable cancel_reason : char option
  }

let leaves o = o.order_qty - o.cum_qty

type displayed =
  { d_unit : int
  ; mutable d_qty : int
  ; mutable d_px : Price4.t
  }

type listing =
  { inst : Instruments.t
  ; lsym : E.Symbol.t
  ; luld : Halts.Luld.t
  ; mutable paused : bool
  ; mutable ssr : Order_types.Ssr.t
  ; mutable ssr_published : bool
  ; sim : Market_sim.t option
  ; mutable auction_orders : order list (** oldest first *)
  }

module User_order = struct
  type t = string * string [@@deriving sexp, compare, hash]
end

type t =
  { config : Config.t
  ; clock : Clock.t
  ; exchange : Exchange.t
  ; instruments : Instruments.t list
  ; listings : listing String.Table.t
  ; live : order Hashtbl.M(User_order).t
  ; used_ids : Hash_set.M(User_order).t
  ; by_engine : order Order_id.Table.t
  ; alias : int Order_id.Table.t (** engine id to PITCH id of re-placed noise orders *)
  ; displayed : displayed Int.Table.t (** PITCH order id to what the feed shows *)
  ; suppressed : Order_id.Hash_set.t (** venue-initiated engine cancels to ignore *)
  ; funded : String.Hash_set.t
  ; reloads : order Queue.t
  ; mutable next_auction_id : int
  ; mutable next_auction_exec : int
  ; mutable last_phase : Phase.t
  ; mutable last_auction_update : Time_ns.t
  ; emit_user : user:string -> unit:int -> Boe.Outbound.t -> unit
  ; emit_feed : unit:int -> Pitch.Message.t -> unit
  }

let instruments t = t.instruments
let clock t = t.clock
let exchange t = t.exchange
let shares s = Size.to_units s / Size.units_per_one
let size_of n = Size.of_int n
let px4 = Instruments.to_price4
let auction_id_base = 1_000_000_000
let auction_exec_base = 12 * Int.pow 36 8
let max_order_qty = 999_999
let user_balance = E.Usd.of_int 1_000_000_000
let now_ns t = Time_ns.to_int_ns_since_epoch (Clock.now t.clock)
let listing t (o : order) = Hashtbl.find_exn t.listings o.inst.symbol

(* BOE output *)

let return_of (o : order) : Boe.Return_fields.t =
  let option_fields =
    match o.inst.kind with
    | Equity _ -> None, None, None
    | Option { osi; _ } ->
      ( Some (Int.of_string (Date.to_string_iso8601_basic osi.expiry))
      , Some (px4 osi.strike)
      , Some
          (match osi.right with
           | Call -> '1'
           | Put -> '0') )
  in
  let maturity_date, strike_price, put_or_call = option_fields in
  let price = Option.map o.price ~f:px4 in
  { Boe.Return_fields.empty with
    side = Some o.side_char
  ; price
  ; exec_inst = o.exec_inst
  ; ord_type = Some o.ord_type
  ; time_in_force = Some o.tif_char
  ; symbol = Some o.inst.symbol
  ; capacity = Some o.capacity
  ; display_indicator = Some (if o.hidden then 'I' else 'V')
  ; max_floor = Some o.max_floor
  ; order_qty = Some o.order_qty
  ; prevent_match = o.prevent_match
  ; maturity_date
  ; strike_price
  ; put_or_call
  ; orig_cl_ord_id = o.orig_cl_ord_id
  ; leaves_qty = Some (leaves o)
  ; working_price = price
  ; display_price = (if o.hidden then None else price)
  }
;;

let send t (o : order) msg = t.emit_user ~user:o.user ~unit:o.inst.unit msg

let ack t o =
  send
    t
    o
    (Order_acknowledgment
       { transaction_time = now_ns t
       ; cl_ord_id = o.cl_ord_id
       ; order_id = o.venue_id
       ; return = return_of o
       })
;;

let send_cancelled t o reason =
  send
    t
    o
    (Order_cancelled
       { transaction_time = now_ns t
       ; cl_ord_id = o.cl_ord_id
       ; reason
       ; return = return_of o
       })
;;

let send_execution t o ~qty ~px ~exec_id ~liquidity =
  send
    t
    o
    (Order_execution
       { transaction_time = now_ns t
       ; cl_ord_id = o.cl_ord_id
       ; exec_id
       ; last_shares = qty
       ; last_px = px
       ; leaves_qty = leaves o
       ; base_liquidity_indicator = liquidity
       ; sub_liquidity_indicator = '\000'
       ; contra_broker = "CBOE"
       ; return = { (return_of o) with last_shares = Some qty; last_px = Some px }
       })
;;

let reject_new t ~user ~unit (n : Boe.New_order.t) ~reason ~text =
  t.emit_user
    ~user
    ~unit
    (Order_rejected
       { transaction_time = now_ns t
       ; cl_ord_id = n.cl_ord_id
       ; reason
       ; text
       ; return =
           { Boe.Return_fields.empty with
             side = Some n.side
           ; price = n.price
           ; symbol = n.symbol
           ; order_qty = Some n.order_qty
           }
       })
;;

(* PITCH output, keyed by the PITCH order id. *)

let feed t ~unit m = t.emit_feed ~unit m

let show t ~unit ~venue_id ~symbol ~side ~qty ~px =
  match Hashtbl.find t.displayed venue_id with
  | None ->
    Hashtbl.set t.displayed ~key:venue_id ~data:{ d_unit = unit; d_qty = qty; d_px = px };
    feed
      t
      ~unit
      (Add_order
         { time_offset = 0
         ; order_id = venue_id
         ; side
         ; quantity = qty
         ; symbol
         ; price = px
         ; flags = 1
         })
  | Some d ->
    if d.d_px = px && qty < d.d_qty
    then (
      feed
        t
        ~unit
        (Reduce_size
           { time_offset = 0; order_id = venue_id; canceled_quantity = d.d_qty - qty });
      d.d_qty <- qty)
    else if d.d_px <> px || qty <> d.d_qty
    then (
      feed
        t
        ~unit
        (Modify_order
           { time_offset = 0; order_id = venue_id; quantity = qty; price = px; flags = 0 });
      d.d_qty <- qty;
      d.d_px <- px)
;;

let hide t ~venue_id =
  match Hashtbl.find_and_remove t.displayed venue_id with
  | None -> ()
  | Some d ->
    feed t ~unit:d.d_unit (Delete_order { time_offset = 0; order_id = venue_id })
;;

let trade_condition (inst : Instruments.t) =
  if Instruments.is_option inst then Some ' ' else None
;;

(* Reports an execution against a resting order: on the displayed order id, else as a
   Trade with an obfuscated id. [at_price] reports an auction execution, whose price is
   not the order's. *)
let report_execution ?at_price t (inst : Instruments.t) ~venue_id ~side ~qty ~px ~exec_id =
  match Hashtbl.find t.displayed venue_id with
  | Some d ->
    d.d_qty <- d.d_qty - qty;
    let remaining = Int.max 0 d.d_qty in
    if remaining = 0 then Hashtbl.remove t.displayed venue_id;
    (match at_price with
     | None ->
       feed
         t
         ~unit:inst.unit
         (Order_executed
            { time_offset = 0
            ; order_id = venue_id
            ; executed_quantity = qty
            ; execution_id = exec_id
            ; trade_condition = trade_condition inst
            })
     | Some price ->
       feed
         t
         ~unit:inst.unit
         (Order_executed_at_price
            { time_offset = 0
            ; order_id = venue_id
            ; executed_quantity = qty
            ; remaining_quantity = remaining
            ; execution_id = exec_id
            ; price
            ; trade_condition = trade_condition inst
            }))
  | None ->
    ignore side;
    feed
      t
      ~unit:inst.unit
      (Trade
         { time_offset = 0
         ; order_id = 0
         ; side = 'B'
         ; quantity = qty
         ; symbol = inst.symbol
         ; price = px
         ; execution_id = exec_id
         ; trade_condition = trade_condition inst
         })
;;

let venue_id_of t oid =
  match Hashtbl.find t.by_engine oid with
  | Some o -> o.venue_id
  | None ->
    (match Hashtbl.find t.alias oid with
     | Some id -> id
     | None -> Order_id.to_int_exn oid)
;;

let is_hidden t oid =
  match Hashtbl.find t.by_engine oid with
  | Some o -> o.hidden
  | None -> false
;;

let side_char (s : E.Side.t) =
  match s with
  | Buy -> 'B'
  | Sell -> 'S'
;;

(* Order life cycle *)

let finish t (o : order) =
  Hashtbl.remove t.live (o.user, o.cl_ord_id);
  Option.iter o.engine_oid ~f:(Hashtbl.remove t.by_engine);
  o.engine_oid <- None
;;

let attach t (o : order) oid =
  Option.iter o.engine_oid ~f:(Hashtbl.remove t.by_engine);
  o.engine_oid <- Some oid;
  Hashtbl.set t.by_engine ~key:oid ~data:o
;;

let date t = fst (Clock.local t.clock)

let on_trade t (l : listing) ~price =
  match l.inst.kind with
  | Option _ -> ()
  | Equity _ ->
    l.ssr
    <- Order_types.Ssr.on_trade
         l.ssr
         ~date:(date t)
         ~prev_close:l.inst.reference_price
         ~price
;;

let cancel_char (reason : Event.Cancel_reason.t) =
  match reason with
  | User -> 'U'
  | Ioc_unfilled -> 'N'
  | Self_trade -> 'V'
  | Liquidated -> 'A'
  | Modified -> 'U'
;;

let process_event t (e : Event.t) =
  match e with
  | Fill { fill; _ } ->
    let qty = shares fill.sz in
    let px = px4 fill.px in
    let exec_id = E.Trade_id.to_int fill.tid in
    let l = Hashtbl.find_exn t.listings (E.Symbol.to_string fill.symbol) in
    on_trade t l ~price:fill.px;
    let maker_venue = venue_id_of t fill.maker_oid in
    if is_hidden t fill.maker_oid then Hashtbl.remove t.displayed maker_venue;
    report_execution
      t
      l.inst
      ~venue_id:maker_venue
      ~side:(Order_book.Fill.maker_side fill)
      ~qty
      ~px
      ~exec_id;
    List.iter
      [ fill.taker_oid, 'R'; fill.maker_oid, 'A' ]
      ~f:(fun (oid, liquidity) ->
        match Hashtbl.find t.by_engine oid with
        | None -> ()
        | Some o ->
          o.cum_qty <- o.cum_qty + qty;
          send_execution t o ~qty ~px ~exec_id ~liquidity)
  | Order_accepted { order; _ } ->
    if not (is_hidden t order.order_id)
    then (
      let l = Hashtbl.find_exn t.listings (E.Symbol.to_string order.symbol) in
      show
        t
        ~unit:l.inst.unit
        ~venue_id:(venue_id_of t order.order_id)
        ~symbol:l.inst.symbol
        ~side:(side_char order.side)
        ~qty:(shares order.size)
        ~px:(px4 order.px))
  | Order_cancelled { order; reason; _ } ->
    let oid = order.order_id in
    if Hash_set.mem t.suppressed oid
    then Hash_set.remove t.suppressed oid
    else (
      match reason with
      | Modified -> ()
      | User | Ioc_unfilled | Self_trade | Liquidated ->
        hide t ~venue_id:(venue_id_of t oid);
        Hashtbl.remove t.alias oid;
        (match Hashtbl.find t.by_engine oid with
         | None -> ()
         | Some o ->
           let reason = Option.value o.cancel_reason ~default:(cancel_char reason) in
           finish t o;
           send_cancelled t o reason))
  | Order_filled { order; _ } ->
    let oid = order.order_id in
    hide t ~venue_id:(venue_id_of t oid);
    Hashtbl.remove t.alias oid;
    (match Hashtbl.find t.by_engine oid with
     | None -> ()
     | Some o ->
       Hashtbl.remove t.by_engine oid;
       o.engine_oid <- None;
       if leaves o > 0 && o.max_floor > 0 then Queue.enqueue t.reloads o else finish t o)
  | Order_rejected _ | Liquidation _ | Funding _ -> ()
;;

let rec drain t =
  List.iter (Exchange.drain_events t.exchange) ~f:(process_event t);
  match Queue.dequeue t.reloads with
  | None -> ()
  | Some o ->
    reload t o;
    drain t

(* A reserve order whose displayed slice filled: a new slice with a new order id. *)
and reload t o =
  let size = Int.min o.max_floor (leaves o) in
  let px = Option.value_exn o.price in
  let req =
    E.Order_request.limit
      ~tif:(if o.post_only then Alo else Gtc)
      o.esym
      (Order_types.Side.to_core o.side)
      ~px
      ~size:(size_of size)
  in
  let oid, status = Exchange.place_with_order_id t.exchange o.account req in
  match status with
  | Rejected _ ->
    finish t o;
    send_cancelled t o 'Z'
  | Resting _ | Filled _ ->
    o.venue_id <- Order_id.to_int_exn oid;
    attach t o oid;
    send
      t
      o
      (Order_restated
         { transaction_time = now_ns t
         ; cl_ord_id = o.cl_ord_id
         ; order_id = o.venue_id
         ; reason = 'R'
         ; return = return_of o
         })
;;

let step t =
  let now = now_ns t in
  if now > Exchange.now t.exchange then Exchange.step t.exchange ~now
;;

let fund t account =
  if not (Hash_set.mem t.funded (E.Account.to_string account))
  then (
    Hash_set.add t.funded (E.Account.to_string account);
    Exchange.deposit t.exchange account user_balance)
;;

(* Validation of a New Order *)

let ( let* ) r f = Result.bind r ~f
let error reason text = Error (reason, text)
let check cond reason text = if cond then Ok () else error reason text

let resolve_symbol t (n : Boe.New_order.t) =
  match n.symbol with
  | None -> error 'A' "Symbol required"
  | Some s ->
    let found =
      match n.maturity_date, n.strike_price, n.put_or_call with
      | Some maturity, Some strike, Some put_or_call ->
        Instruments.find_option t.instruments ~root:s ~maturity ~strike ~put_or_call
      | _ -> Instruments.find t.instruments s
    in
    (match found with
     | Some i -> Ok i
     | None -> error 'Y' "symbol not supported")
;;

let parse_side = function
  | '1' -> Ok Order_types.Side.Buy
  | '2' -> Ok Sell
  | '5' -> Ok Sell_short
  | '6' -> Ok Sell_short_exempt
  | c -> error 'A' (sprintf "invalid Side %C" c)
;;

let parse_tif = function
  | '0' | '1' -> Ok (Day, Order_types.Tif.Day)
  | '3' -> Ok (Ioc, Ioc)
  | '4' -> Ok (Fok, Fok)
  | '2' -> Ok (Opg, Opg)
  | '7' -> Ok (Clo, Clo)
  | c -> error 'A' (sprintf "unsupported TimeInForce %C" c)
;;

let parse_stp = function
  | None -> Order_types.Stp.Allow
  | Some s when String.is_empty s -> Allow
  | Some s ->
    (match s.[0] with
     | 'O' -> Cancel_oldest
     | 'B' -> Cancel_both
     | 'D' -> Decrement_and_cancel
     | _ -> Cancel_newest)
;;

let bands (l : listing) ~at ~reference =
  match l.inst.kind with
  | Option _ -> None
  | Equity { tier } -> Some (Halts.luld_bands tier ~reference ~at)
;;

let reference_price t (l : listing) =
  Option.value (Exchange.mark t.exchange l.lsym) ~default:l.inst.reference_price
;;

let best_bid t (l : listing) =
  Option.bind (Exchange.book t.exchange l.lsym) ~f:Order_book.best_bid
;;

let make_order t ~user (n : Boe.New_order.t) =
  let* () = check (not (String.is_empty n.cl_ord_id)) 'A' "ClOrdID required" in
  let* () =
    check (not (Hash_set.mem t.used_ids (user, n.cl_ord_id))) 'D' "duplicate ClOrdID"
  in
  let* inst = resolve_symbol t n in
  let* side = parse_side n.side in
  let* () = check (n.order_qty > 0) 'A' "OrderQty must be positive" in
  let* () = check (n.order_qty <= max_order_qty) 'M' "OrderQty above 999,999" in
  let* () = check (Option.is_some n.capacity) 'A' "Capacity required" in
  let ord_type = Option.value n.ord_type ~default:'2' in
  let* () =
    check
      (Char.equal ord_type '1' || Char.equal ord_type '2')
      'A'
      (sprintf "unsupported OrdType %C" ord_type)
  in
  let tif_char = Option.value n.time_in_force ~default:'0' in
  let* tif, ot_tif = parse_tif tif_char in
  let* limit =
    match ord_type, n.price with
    | '1', _ -> Ok None
    | _, None -> error 'A' "Price required for a limit order"
    | _, Some p ->
      let px = Instruments.of_price4 p in
      let* () =
        check
          (Price.is_positive px && Instruments.is_valid_price inst px)
          'A'
          "Price not on the tick"
      in
      Ok (Some px)
  in
  let routing =
    Option.value_map n.routing_inst ~default:'B' ~f:(fun s ->
      if String.is_empty s then 'B' else s.[0])
  in
  let post_only = Char.equal routing 'P' in
  let hidden = Option.value_map n.display_indicator ~default:false ~f:(Char.equal 'I') in
  let max_floor = Option.value n.max_floor ~default:0 in
  let display : Order_types.Display.t =
    if hidden
    then Hidden
    else if max_floor > 0 && max_floor < n.order_qty
    then Reserve { display_qty = max_floor }
    else Visible
  in
  let ot : Order_types.t =
    { side
    ; qty = n.order_qty
    ; limit
    ; tif = ot_tif
    ; display
    ; iso = Option.value_map n.exec_inst ~default:false ~f:(Char.equal 'f')
    ; post_only
    ; min_qty = None
    ; stp = parse_stp n.prevent_match
    }
  in
  let* () =
    match inst.kind with
    | Equity _ ->
      (* Reserve orders on Cboe need not be round lots; validate the rest. *)
      Order_types.validate ~round_lot:1 ot
      |> Result.map_error ~f:(fun e -> 'A', Error.to_string_hum e)
    | Option _ -> Ok ()
  in
  let account = E.Account.of_string ("boe-" ^ user) in
  Ok
    ( { user
      ; account
      ; inst
      ; esym = E.Symbol.of_string inst.symbol
      ; side
      ; side_char = n.side
      ; cl_ord_id = n.cl_ord_id
      ; orig_cl_ord_id = None
      ; venue_id = 0
      ; engine_oid = None
      ; price = limit
      ; order_qty = n.order_qty
      ; cum_qty = 0
      ; ord_type
      ; tif_char
      ; tif
      ; capacity = Option.value_exn n.capacity
      ; max_floor =
          (match display with
           | Reserve _ -> max_floor
           | Visible | Hidden -> 0)
      ; hidden
      ; post_only
      ; prevent_match = n.prevent_match
      ; exec_inst = n.exec_inst
      ; cancel_reason = None
      }
    , ot )
;;

(* Session, halt, band and short sale checks that depend on market state. *)
let market_checks t (o : order) =
  let l = listing t o in
  let phase = Clock.phase t.clock in
  let has_sessions = Clock.has_sessions t.clock in
  let* () = check (not (Phase.equal phase Closed)) 'A' "market closed" in
  let* () =
    check
      ((not (Instruments.is_option o.inst)) || Phase.equal phase Core)
      'A'
      "options trade in core hours only"
  in
  let* () = check (not l.paused) 'H' "symbol halted (LULD pause)" in
  let* () =
    match o.tif with
    | Opg ->
      check
        (has_sessions
         && Phase.equal phase Pre_market
         && not (Instruments.is_option o.inst))
        'A'
        "on-open orders are accepted before the opening auction only"
    | Clo ->
      check
        (has_sessions
         && (Phase.equal phase Pre_market || Phase.equal phase Core)
         && not (Instruments.is_option o.inst))
        'A'
        "on-close orders are accepted before the closing auction only"
    | Day | Ioc | Fok ->
      check
        (Char.(o.ord_type <> '1') || Phase.equal phase Core)
        'A'
        "market orders are accepted in the core session only"
  in
  let* () =
    match o.price, bands l ~at:(Clock.now t.clock) ~reference:(reference_price t l) with
    | Some px, Some b
      when Phase.equal phase Core && not (equal_tif o.tif Opg || equal_tif o.tif Clo) ->
      let ok =
        match Order_types.Side.to_core o.side with
        | Buy -> Price.(px <= b.upper)
        | Sell -> Price.(px >= b.lower)
      in
      check ok 'u' "price outside the LULD bands"
    | _ -> Ok ()
  in
  match o.side with
  | Sell_short ->
    let ok =
      match best_bid t l with
      | None -> true
      | Some national_best_bid ->
        (match o.price with
         | None -> not (Order_types.Ssr.is_active l.ssr ~date:(date t))
         | Some price ->
           Order_types.Ssr.allows
             l.ssr
             ~date:(date t)
             ~side:o.side
             ~price
             ~national_best_bid)
    in
    check ok 'S' "short sale price test (Rule 201)"
  | Buy | Sell | Sell_short_exempt -> Ok ()
;;

let register t (o : order) =
  Hash_set.add t.used_ids (o.user, o.cl_ord_id);
  Hashtbl.set t.live ~key:(o.user, o.cl_ord_id) ~data:o
;;

let crosses (o : order) (other : order) =
  E.Side.equal
    (Order_types.Side.to_core other.side)
    (E.Side.flip (Order_types.Side.to_core o.side))
  && E.Symbol.equal other.esym o.esym
  && Option.is_some other.engine_oid
  &&
  match o.price, other.price with
  | None, _ -> true
  | Some _, None -> false
  | Some mine, Some theirs ->
    (match Order_types.Side.to_core o.side with
     | Buy -> Price.(theirs <= mine)
     | Sell -> Price.(theirs >= mine))
;;

let cancel_engine t (o : order) ~reason =
  match o.engine_oid with
  | None -> ()
  | Some oid ->
    o.cancel_reason <- Some reason;
    ignore (Exchange.cancel t.exchange o.account ~symbol:o.esym oid : E.Cancel_status.t)
;;

(* Self-trade prevention the engine does not do: cancel the resting side ([O]) or both
   ([B]). Returns whether the new order may proceed. *)
let prevent_self_trade t (o : order) (ot : Order_types.t) =
  match ot.stp with
  | Cancel_oldest | Cancel_both ->
    let mine =
      Hashtbl.data t.live
      |> List.filter ~f:(fun other -> String.equal other.user o.user && crosses o other)
    in
    List.iter mine ~f:(fun other -> cancel_engine t other ~reason:'V');
    drain t;
    (match ot.stp with
     | Cancel_both -> List.is_empty mine
     | _ -> true)
  | Allow | Cancel_newest | Decrement_and_cancel -> true
;;

let available_liquidity t (o : order) =
  match Exchange.book t.exchange o.esym with
  | None -> 0
  | Some book ->
    let bids, asks = Order_book.l2 book ~n_levels:1000 in
    let levels =
      match Order_types.Side.to_core o.side with
      | Buy -> asks
      | Sell -> bids
    in
    List.filter levels ~f:(fun (lvl : E.L2.Level.t) ->
      match o.price with
      | None -> true
      | Some px ->
        (match Order_types.Side.to_core o.side with
         | Buy -> Price.(lvl.px <= px)
         | Sell -> Price.(lvl.px >= px)))
    |> List.sum (module Int) ~f:(fun (lvl : E.L2.Level.t) -> shares lvl.sz)
;;

let engine_request (o : order) (ot : Order_types.t) ~size =
  let view : Order_types.t =
    { ot with
      qty = size
    ; display = Visible
    ; tif =
        (match ot.tif with
         | Fok -> Ioc
         | tif -> tif)
    ; min_qty = None
    }
  in
  Order_types.to_order_request view ~symbol:o.esym
;;

let submit t (o : order) (ot : Order_types.t) =
  let size = if o.max_floor > 0 then Int.min o.max_floor o.order_qty else o.order_qty in
  match engine_request o ot ~size with
  | Error e ->
    finish t o;
    send_cancelled t o 'Z';
    ignore e
  | Ok req ->
    drain t;
    let oid, status = Exchange.place_with_order_id t.exchange o.account req in
    o.venue_id <- Order_id.to_int_exn oid;
    (match status with
     | Resting _ | Filled _ ->
       attach t o oid;
       ack t o;
       drain t
     | Rejected ((No_liquidity | Self_trade) as r) ->
       ack t o;
       drain t;
       finish t o;
       send_cancelled
         t
         o
         (match r with
          | Self_trade -> 'V'
          | _ -> 'N')
     | Rejected r ->
       drain t;
       Hashtbl.remove t.live (o.user, o.cl_ord_id);
       let reason, text =
         match r with
         | Post_only_would_cross _ -> 'W', "post only order would remove liquidity"
         | Insufficient_margin -> 'A', "insufficient buying power"
         | Invalid_price -> 'A', "invalid price"
         | Invalid_size -> 'A', "invalid size"
         | Unknown_symbol -> 'Y', "symbol not supported"
         | r -> 'Z', Sexp.to_string (E.Reject_reason.sexp_of_t r)
       in
       send
         t
         o
         (Order_rejected
            { transaction_time = now_ns t
            ; cl_ord_id = o.cl_ord_id
            ; reason
            ; text
            ; return = return_of o
            }))
;;

let new_order t ~user (n : Boe.New_order.t) =
  step t;
  let unit_of n =
    match resolve_symbol t n with
    | Ok i -> i.unit
    | Error _ -> Instruments.equities_unit
  in
  match
    let* o, ot = make_order t ~user n in
    let* () = market_checks t o in
    Ok (o, ot)
  with
  | Error (reason, text) -> reject_new t ~user ~unit:(unit_of n) n ~reason ~text
  | Ok (o, ot) ->
    fund t o.account;
    register t o;
    (match o.tif with
     | Opg | Clo ->
       let l = listing t o in
       o.venue_id <- t.next_auction_id;
       t.next_auction_id <- t.next_auction_id + 1;
       l.auction_orders <- l.auction_orders @ [ o ];
       ack t o
     | Fok when available_liquidity t o < o.order_qty ->
       o.venue_id <- t.next_auction_id;
       t.next_auction_id <- t.next_auction_id + 1;
       ack t o;
       finish t o;
       send_cancelled t o 'N'
     | Day | Ioc | Fok ->
       if prevent_self_trade t o ot
       then submit t o ot
       else (
         o.venue_id <- t.next_auction_id;
         t.next_auction_id <- t.next_auction_id + 1;
         ack t o;
         finish t o;
         send_cancelled t o 'V'))
;;

let is_auction_order (o : order) = equal_tif o.tif Opg || equal_tif o.tif Clo

let remove_auction_order t (o : order) =
  let l = listing t o in
  l.auction_orders <- List.filter l.auction_orders ~f:(fun x -> not (phys_equal x o))
;;

let cancel_order t ~user (c : Boe.Cancel_order.t) =
  step t;
  let reject reason text =
    t.emit_user
      ~user
      ~unit:Instruments.equities_unit
      (Cancel_rejected
         { transaction_time = now_ns t
         ; cl_ord_id = c.orig_cl_ord_id
         ; reason
         ; text
         ; return = Boe.Return_fields.empty
         })
  in
  match Hashtbl.find t.live (user, c.orig_cl_ord_id) with
  | None ->
    if Hash_set.mem t.used_ids (user, c.orig_cl_ord_id)
    then reject 'J' "too late to cancel"
    else reject 'O' "unknown ClOrdID"
  | Some o when is_auction_order o ->
    remove_auction_order t o;
    finish t o;
    send_cancelled t o 'U'
  | Some o ->
    (match o.engine_oid with
     | None -> reject 'J' "too late to cancel"
     | Some oid ->
       drain t;
       (match Exchange.cancel t.exchange o.account ~symbol:o.esym oid with
        | Cancelled -> drain t
        | Not_found -> reject 'J' "too late to cancel"))
;;

let modify_order t ~user (m : Boe.Modify_order.t) =
  step t;
  let reject reason text =
    t.emit_user
      ~user
      ~unit:Instruments.equities_unit
      (User_modify_rejected
         { transaction_time = now_ns t
         ; cl_ord_id = m.cl_ord_id
         ; reason
         ; text
         ; return =
             { Boe.Return_fields.empty with orig_cl_ord_id = Some m.orig_cl_ord_id }
         })
  in
  match Hashtbl.find t.live (user, m.orig_cl_ord_id) with
  | None ->
    if Hash_set.mem t.used_ids (user, m.orig_cl_ord_id)
    then reject 'J' "too late to modify"
    else reject 'O' "unknown OrigClOrdID"
  | Some o ->
    let l = listing t o in
    let new_qty = Option.value m.order_qty ~default:o.order_qty in
    let new_px =
      Option.value_map m.price ~default:o.price ~f:(fun p ->
        Some (Instruments.of_price4 p))
    in
    let result =
      let* () =
        check (not (Hash_set.mem t.used_ids (user, m.cl_ord_id))) 'D' "duplicate ClOrdID"
      in
      let* () =
        check
          (Option.value_map m.side ~default:true ~f:(Char.equal o.side_char))
          'A'
          "side cannot be modified"
      in
      let* () = check (new_qty <= max_order_qty) 'M' "OrderQty above 999,999" in
      let* () =
        check (new_qty > o.cum_qty) 'A' "OrderQty not above the executed quantity"
      in
      let* () =
        match new_px with
        | Some px ->
          check (Instruments.is_valid_price o.inst px) 'A' "Price not on the tick"
        | None -> check (Option.is_none m.price) 'A' "market orders cannot be repriced"
      in
      let* () = check (not l.paused) 'H' "symbol halted (LULD pause)" in
      match new_px, bands l ~at:(Clock.now t.clock) ~reference:(reference_price t l) with
      | Some px, Some b
        when Phase.equal (Clock.phase t.clock) Core && not (is_auction_order o) ->
        check
          (match Order_types.Side.to_core o.side with
           | Buy -> Price.(px <= b.upper)
           | Sell -> Price.(px >= b.lower))
          'u'
          "price outside the LULD bands"
      | _ -> Ok ()
    in
    (match result with
     | Error (reason, text) -> reject reason text
     | Ok () ->
       Hashtbl.remove t.live (user, o.cl_ord_id);
       o.orig_cl_ord_id <- Some o.cl_ord_id;
       o.cl_ord_id <- m.cl_ord_id;
       register t o;
       o.order_qty <- new_qty;
       o.price <- new_px;
       let modified () =
         send
           t
           o
           (Order_modified
              { transaction_time = now_ns t
              ; cl_ord_id = o.cl_ord_id
              ; order_id = o.venue_id
              ; return = return_of o
              })
       in
       (match o.engine_oid with
        | None -> modified ()
        | Some old ->
          drain t;
          let size =
            if o.max_floor > 0 then Int.min o.max_floor (leaves o) else leaves o
          in
          let req =
            E.Order_request.limit
              ~tif:(if o.post_only then Alo else Gtc)
              o.esym
              (Order_types.Side.to_core o.side)
              ~px:(Option.value_exn o.price)
              ~size:(size_of size)
          in
          Hashtbl.remove t.by_engine old;
          o.engine_oid <- None;
          (match Exchange.modify t.exchange o.account (Order_id old) req with
           | Resting { order_id; _ } | Filled { order_id; _ } ->
             Hashtbl.set t.by_engine ~key:order_id ~data:o;
             o.engine_oid <- Some order_id;
             modified ();
             drain t
           | Rejected _ ->
             drain t;
             hide t ~venue_id:o.venue_id;
             finish t o;
             send_cancelled t o 'Z')))
;;

(* Auctions *)

let kind_char (k : Auction.Kind.t) =
  match k with
  | Opening -> 'O'
  | Closing -> 'C'
  | Halt_reopening -> 'H'
;;

let auction_participants t (l : listing) ~(kind : Auction.Kind.t) =
  let wanted =
    match kind with
    | Opening -> Opg
    | Closing | Halt_reopening -> Clo
  in
  let auction_only =
    List.filter l.auction_orders ~f:(fun o -> equal_tif o.tif wanted)
    |> List.map ~f:(fun o ->
      { Auction.Order.id = o.venue_id
      ; side = Order_types.Side.to_core o.side
      ; qty = leaves o
      ; kind =
          (match kind, o.price with
           | Opening, None -> Moo
           | Opening, Some _ -> Loo
           | _, None -> Moc
           | _, Some _ -> Loc)
      ; limit = o.price
      })
  in
  let book =
    match Exchange.book t.exchange l.lsym with
    | None -> []
    | Some b ->
      Order_book.orders b
      |> List.map ~f:(fun (r : Order_book.Resting_order.t) ->
        { Auction.Order.id = Order_id.to_int_exn r.oid
        ; side = r.side
        ; qty = shares r.remaining
        ; kind = Book
        ; limit = Some r.px
        })
  in
  auction_only, book
;;

let next_auction_exec t =
  t.next_auction_exec <- t.next_auction_exec + 1;
  auction_exec_base + t.next_auction_exec
;;

let auction_fill_book_order t (l : listing) (fill : Auction.Fill.t) =
  let oid = Order_id.of_int fill.id in
  match
    Option.bind (Exchange.book t.exchange l.lsym) ~f:(fun b -> Order_book.find b oid)
  with
  | None -> ()
  | Some r ->
    let remaining = shares r.remaining - fill.qty in
    let exec_id = next_auction_exec t in
    let price = px4 fill.price in
    let venue_id = venue_id_of t oid in
    let boe = Hashtbl.find t.by_engine oid in
    Hash_set.add t.suppressed oid;
    ignore (Exchange.cancel t.exchange r.owner ~symbol:l.lsym oid : E.Cancel_status.t);
    drain t;
    if is_hidden t oid then Hashtbl.remove t.displayed venue_id;
    report_execution
      ~at_price:price
      t
      l.inst
      ~venue_id
      ~side:r.side
      ~qty:fill.qty
      ~px:price
      ~exec_id;
    Option.iter boe ~f:(fun o ->
      Hashtbl.remove t.by_engine oid;
      o.engine_oid <- None;
      o.cum_qty <- o.cum_qty + fill.qty;
      send_execution t o ~qty:fill.qty ~px:price ~exec_id ~liquidity:'C');
    if remaining > 0
    then (
      (* The engine cannot shrink an order in place: re-place the remainder under the same
         PITCH order id. *)
      let new_oid, status =
        Exchange.place_with_order_id
          t.exchange
          r.owner
          (E.Order_request.limit l.lsym r.side ~px:r.px ~size:(size_of remaining))
      in
      (match status, boe with
       | (Resting _ | Filled _), Some o -> attach t o new_oid
       | (Resting _ | Filled _), None -> Hashtbl.set t.alias ~key:new_oid ~data:venue_id
       | Rejected _, Some o ->
         finish t o;
         send_cancelled t o 'Z'
       | Rejected _, None -> hide t ~venue_id);
      drain t)
    else
      Option.iter boe ~f:(fun o ->
        if leaves o > 0 && o.max_floor > 0
        then (
          Queue.enqueue t.reloads o;
          drain t)
        else finish t o)
;;

let run_auction t (l : listing) ~(kind : Auction.Kind.t) =
  drain t;
  let auction_only, book = auction_participants t l ~kind in
  let reference = reference_price t l in
  let by_id = Int.Table.create () in
  List.iter l.auction_orders ~f:(fun o -> Hashtbl.set by_id ~key:o.venue_id ~data:o);
  let price, matched =
    match Auction.uncross ~kind ~reference (auction_only @ book) with
    | None -> reference, 0
    | Some (result, fills) ->
      List.iter fills ~f:(fun (fill : Auction.Fill.t) ->
        if fill.id >= auction_id_base
        then
          Option.iter (Hashtbl.find by_id fill.id) ~f:(fun o ->
            o.cum_qty <- o.cum_qty + fill.qty;
            send_execution
              t
              o
              ~qty:fill.qty
              ~px:(px4 fill.price)
              ~exec_id:(next_auction_exec t)
              ~liquidity:'C')
        else auction_fill_book_order t l fill);
      if result.matched > 0
      then (
        on_trade t l ~price:result.price;
        Exchange.set_mark t.exchange l.lsym result.price);
      result.price, result.matched
  in
  if not (List.is_empty auction_only && matched = 0)
  then
    feed
      t
      ~unit:l.inst.unit
      (Auction_summary
         { time_offset = 0
         ; symbol = l.inst.symbol
         ; auction_type = kind_char kind
         ; price = px4 price
         ; shares = matched
         });
  (* Unexecuted on-open / on-close orders are cancelled after their auction. *)
  let wanted =
    match kind with
    | Opening -> Opg
    | Closing | Halt_reopening -> Clo
  in
  let done_, rest =
    List.partition_tf l.auction_orders ~f:(fun o -> equal_tif o.tif wanted)
  in
  l.auction_orders <- rest;
  List.iter done_ ~f:(fun o ->
    finish t o;
    if leaves o > 0 then send_cancelled t o 'X')
;;

let auction_update t (l : listing) ~(kind : Auction.Kind.t) =
  let auction_only, book = auction_participants t l ~kind in
  if not (List.is_empty auction_only)
  then (
    let reference = reference_price t l in
    let imb = Auction.imbalance ~kind ~reference (auction_only @ book) in
    let buy, sell =
      match imb.imbalance_side with
      | Some Buy -> imb.paired_qty + imb.imbalance_qty, imb.paired_qty
      | Some Sell -> imb.paired_qty, imb.paired_qty + imb.imbalance_qty
      | None -> imb.paired_qty, imb.paired_qty
    in
    let auction_only_price =
      Auction.clearing ~kind ~reference auction_only
      |> Option.value_map ~default:0 ~f:(fun (r : Auction.Result.t) -> px4 r.price)
    in
    feed
      t
      ~unit:l.inst.unit
      (Auction_update
         { time_offset = 0
         ; symbol = l.inst.symbol
         ; auction_type = kind_char kind
         ; reference_price = px4 reference
         ; buy_shares = buy
         ; sell_shares = sell
         ; indicative_price = Option.value_map imb.indicative_price ~default:0 ~f:px4
         ; auction_only_price
         }))
;;

let trading_status t (inst : Instruments.t) : Pitch.Message.t =
  let l = Hashtbl.find_exn t.listings inst.symbol in
  Trading_status
    { time_offset = 0
    ; symbol = inst.symbol
    ; status = (if l.paused then 'H' else 'T')
    ; reg_sho_action = (if l.ssr_published then '1' else '0')
    }
;;

let equities t =
  Hashtbl.data t.listings
  |> List.filter ~f:(fun l -> not (Instruments.is_option l.inst))
  |> List.sort ~compare:(fun a b -> String.compare a.inst.symbol b.inst.symbol)
;;

let all_listings t =
  List.map t.instruments ~f:(fun i -> Hashtbl.find_exn t.listings i.symbol)
;;

let expire t ~(f : order -> bool) =
  Hashtbl.data t.live
  |> List.filter ~f:(fun o -> f o && not (is_auction_order o))
  |> List.iter ~f:(fun o -> cancel_engine t o ~reason:'X');
  drain t
;;

let close_session t =
  expire t ~f:(fun _ -> true);
  List.iter (all_listings t) ~f:(fun l ->
    List.iter l.auction_orders ~f:(fun o ->
      finish t o;
      send_cancelled t o 'X');
    l.auction_orders <- [];
    Option.iter (Exchange.book t.exchange l.lsym) ~f:(fun b ->
      List.iter (Order_book.orders b) ~f:(fun r ->
        ignore
          (Exchange.cancel t.exchange r.owner ~symbol:l.lsym r.oid : E.Cancel_status.t))));
  drain t;
  List.iter [ Instruments.equities_unit; Instruments.options_unit ] ~f:(fun unit ->
    feed t ~unit (End_of_session { time_offset = 0 }))
;;

let on_phase_change t ~(from : Phase.t) ~(to_ : Phase.t) =
  match from, to_ with
  | (Pre_market | Closed), Core ->
    List.iter (equities t) ~f:(fun l -> run_auction t l ~kind:Opening)
  | Core, (Post_market | Closed) ->
    List.iter (equities t) ~f:(fun l -> run_auction t l ~kind:Closing);
    expire t ~f:(fun o -> equal_tif o.tif Day);
    if Phase.equal to_ Closed then close_session t
  | Post_market, Closed | Pre_market, Closed -> close_session t
  | Closed, (Pre_market | Post_market) ->
    List.iter (all_listings t) ~f:(fun l ->
      l.paused <- false;
      feed t ~unit:l.inst.unit (trading_status t l.inst))
  | _ -> ()
;;

let in_auction_window t ~(phase : Phase.t) =
  let date, tod = Clock.local t.clock in
  match Session.Us_equities.hours date with
  | None -> None
  | Some h ->
    let lead = t.config.auction_lead in
    (match phase with
     | Pre_market when Time_ns.Span.(tod >= h.core_open - lead) ->
       Some Auction.Kind.Opening
     | Core when Time_ns.Span.(tod >= h.core_close - lead) -> Some Closing
     | _ -> None)
;;

let luld t (l : listing) =
  match bands l ~at:(Clock.now t.clock) ~reference:(reference_price t l) with
  | None -> ()
  | Some bands ->
    let now = Clock.now t.clock in
    let book = Exchange.book t.exchange l.lsym in
    Halts.Luld.on_quote
      l.luld
      ~now
      ~bands
      ~best_bid:(Option.bind book ~f:Order_book.best_bid)
      ~best_offer:(Option.bind book ~f:Order_book.best_ask);
    let paused =
      match Halts.Luld.state l.luld with
      | Paused _ -> true
      | Normal | Limit_state _ -> false
    in
    if Bool.(paused <> l.paused)
    then (
      l.paused <- paused;
      feed t ~unit:l.inst.unit (trading_status t l.inst))
;;

let ssr_status t (l : listing) =
  let active = Order_types.Ssr.is_active l.ssr ~date:(date t) in
  if Bool.(active <> l.ssr_published)
  then (
    l.ssr_published <- active;
    feed t ~unit:l.inst.unit (trading_status t l.inst))
;;

let tick t =
  step t;
  drain t;
  let phase = Clock.phase t.clock in
  if Clock.has_sessions t.clock && not (Phase.equal phase t.last_phase)
  then (
    let from = t.last_phase in
    t.last_phase <- phase;
    on_phase_change t ~from ~to_:phase);
  if Clock.has_sessions t.clock
  then (
    let now = Time_ns.now () in
    if Time_ns.Span.(Time_ns.diff now t.last_auction_update >= second)
    then (
      t.last_auction_update <- now;
      Option.iter (in_auction_window t ~phase) ~f:(fun kind ->
        List.iter (equities t) ~f:(fun l -> auction_update t l ~kind))));
  if not (Phase.equal phase Closed)
  then (
    List.iter (all_listings t) ~f:(fun l ->
      if Phase.equal phase Core || not (Instruments.is_option l.inst)
      then
        if not l.paused
        then (
          Option.iter l.sim ~f:Market_sim.tick;
          drain t));
    if Phase.equal phase Core
    then
      List.iter (equities t) ~f:(fun l ->
        luld t l;
        ssr_status t l))
;;

let create ?(config = Config.default) ~clock ~emit_user ~emit_feed instruments =
  let exchange =
    Exchange.create
      ~config:
        { instruments = List.map instruments ~f:Instruments.to_engine
        ; default_leverage = 4
        ; funding_interval_ns = Int.max_value / 4
        ; default_funding_rate = 0.
        ; min_order_value = E.Usd.one
        ; market_slippage_bps = 500
        ; l2_levels = 10
        }
      ()
  in
  let t =
    { config
    ; clock
    ; exchange
    ; instruments
    ; listings = String.Table.create ()
    ; live = Hashtbl.create (module User_order)
    ; used_ids = Hash_set.create (module User_order)
    ; by_engine = Order_id.Table.create ()
    ; alias = Order_id.Table.create ()
    ; displayed = Int.Table.create ()
    ; suppressed = Order_id.Hash_set.create ()
    ; funded = String.Hash_set.create ()
    ; reloads = Queue.create ()
    ; next_auction_id = auction_id_base
    ; next_auction_exec = 0
    ; last_phase = Clock.phase clock
    ; last_auction_update = Time_ns.epoch
    ; emit_user
    ; emit_feed
    }
  in
  step t;
  List.iteri instruments ~f:(fun i inst ->
    let lsym = E.Symbol.of_string inst.symbol in
    let sim =
      if config.noise && config.noise_traders > 0
      then (
        let min_notional, max_notional =
          if Instruments.is_option inst then 20., 400. else 2_000., 40_000.
        in
        Some
          (Market_sim.create
             { Market_sim.Config.default with
               seed = config.seed + i
             ; symbols = [ lsym ]
             ; initial_prices = [ lsym, inst.reference_price ]
             ; noise_traders = config.noise_traders
             ; trader_balance = E.Usd.of_int 100_000_000
             ; min_notional
             ; max_notional
             }
             exchange))
      else None
    in
    Hashtbl.set
      t.listings
      ~key:inst.symbol
      ~data:
        { inst
        ; lsym
        ; luld = Halts.Luld.create ()
        ; paused = false
        ; ssr = Order_types.Ssr.inactive
        ; ssr_published = false
        ; sim
        ; auction_orders = []
        });
  t
;;

let open_order_count t ~user = Hashtbl.count t.live ~f:(fun o -> String.equal o.user user)
