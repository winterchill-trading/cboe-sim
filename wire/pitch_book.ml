open! Core

module Order = struct
  type t =
    { order_id : int
    ; symbol : string
    ; side : char
    ; quantity : int
    ; price : Price4.t
    }
  [@@deriving sexp, compare, equal]
end

module Key = struct
  type t = int * int [@@deriving sexp, compare, hash]
end

type entry =
  { arrival : int
  ; order : Order.t
  }

type t =
  { orders : entry Hashtbl.M(Key).t
  ; mutable next_arrival : int
  }

let create () = { orders = Hashtbl.create (module Key); next_arrival = 0 }

let update t key ~f =
  match Hashtbl.find t.orders key with
  | None -> ()
  | Some e ->
    (match f e.order with
     | None -> Hashtbl.remove t.orders key
     | Some order -> Hashtbl.set t.orders ~key ~data:{ e with order })
;;

let reduce (o : Order.t) by =
  let quantity = o.quantity - by in
  if quantity <= 0 then None else Some { o with quantity }
;;

let apply t ~unit (m : Pitch.Message.t) =
  match m with
  | Add_order { order_id; side; quantity; symbol; price; _ } ->
    t.next_arrival <- t.next_arrival + 1;
    Hashtbl.set
      t.orders
      ~key:(unit, order_id)
      ~data:
        { arrival = t.next_arrival; order = { order_id; symbol; side; quantity; price } }
  | Order_executed { order_id; executed_quantity; _ } ->
    update t (unit, order_id) ~f:(fun o -> reduce o executed_quantity)
  | Order_executed_at_price { order_id; remaining_quantity; _ } ->
    update t (unit, order_id) ~f:(fun o ->
      if remaining_quantity <= 0
      then None
      else Some { o with quantity = remaining_quantity })
  | Reduce_size { order_id; canceled_quantity; _ } ->
    update t (unit, order_id) ~f:(fun o -> reduce o canceled_quantity)
  | Modify_order { order_id; quantity; price; _ } ->
    update t (unit, order_id) ~f:(fun o ->
      if quantity <= 0 then None else Some { o with quantity; price })
  | Delete_order { order_id; _ } -> Hashtbl.remove t.orders (unit, order_id)
  | Time _
  | Trade _
  | End_of_session _
  | Trading_status _
  | Auction_update _
  | Auction_summary _
  | Symbol_mapping _
  | Spin_response _
  | Spin_finished _
  | Unknown _ -> ()
;;

let entries t ~unit =
  Hashtbl.fold t.orders ~init:[] ~f:(fun ~key:(u, _) ~data acc ->
    if u = unit then data :: acc else acc)
  |> List.sort ~compare:(fun a b -> Int.compare a.arrival b.arrival)
;;

let orders t ~unit = List.map (entries t ~unit) ~f:(fun e -> e.order)
let find t ~unit id = Hashtbl.find t.orders (unit, id) |> Option.map ~f:(fun e -> e.order)

let levels t ~symbol ~bids =
  let side = if bids then 'B' else 'S' in
  Hashtbl.fold t.orders ~init:Int.Map.empty ~f:(fun ~key:_ ~data:{ order; _ } acc ->
    if String.equal order.symbol symbol && Char.equal order.side side
    then
      Map.update acc order.price ~f:(function
        | None -> order.quantity, 1
        | Some (q, n) -> q + order.quantity, n + 1)
    else acc)
  |> Map.to_alist ~key_order:(if bids then `Decreasing else `Increasing)
  |> List.map ~f:(fun (px, (q, n)) -> px, q, n)
;;
