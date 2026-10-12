open! Core
open! Async
open! Exchange_core
module Market_sim = Exchange_core_engine.Market_sim

let scale_price px ~factor =
  if Float.equal factor 1.
  then px
  else
    Price.of_units (Float.iround_nearest_exn (Float.of_int (Price.to_units px) *. factor))
;;

let check_factor factor =
  if Float.is_finite factor && Float.( > ) factor 0.
  then Ok ()
  else Or_error.error_s [%message "price factor must be positive" (factor : float)]
;;

module Target = struct
  type t =
    | Series of (Time_ns.t * Price.t) array
    | Fixed of
        { price : Price.t
        ; drift_bps_per_day : float
        }
  [@@deriving sexp_of]

  let positive px =
    if Price.( > ) px Price.zero
    then Ok ()
    else Or_error.error_s [%message "reference price must be positive" (px : Price.t)]
  ;;

  let series points =
    let open Or_error.Let_syntax in
    let%bind () = Or_error.all_unit (List.map points ~f:(fun (_, px) -> positive px)) in
    match List.stable_sort points ~compare:(fun (a, _) (b, _) -> Time_ns.compare a b) with
    | [] -> Or_error.error_string "empty reference series"
    | points -> Ok (Series (Array.of_list points))
  ;;

  let fixed ?(drift_bps_per_day = 0.) price =
    let open Or_error.Let_syntax in
    let%bind () = positive price in
    if Float.is_finite drift_bps_per_day
    then Ok (Fixed { price; drift_bps_per_day })
    else Or_error.error_s [%message "drift must be finite" (drift_bps_per_day : float)]
  ;;

  let parse_fixed s =
    Or_error.try_with_join (fun () ->
      match String.split s ~on:',' with
      | [ px ] -> fixed (Price.of_string px)
      | [ px; drift ] ->
        fixed ~drift_bps_per_day:(Float.of_string drift) (Price.of_string px)
      | _ -> Or_error.error_string "expected <price>[,<drift_bps_per_day>]")
    |> Or_error.tag_s ~tag:[%message "fixed anchor" s]
  ;;

  let load_series ?(factor = 1.) path =
    match check_factor factor with
    | Error _ as e -> return e
    | Ok () ->
      (match%bind Price_anchor.Source.of_series path with
       | Error _ as e -> return e
       | Ok pipe ->
         let%map items = Pipe.to_list pipe in
         List.filter_map items ~f:(function
           | Price { recv_ts; price } -> Some (recv_ts, scale_price price ~factor)
           | Event _ | Gap _ -> None)
         |> series)
  ;;

  let event_price : Market_data.Event.t -> _ = function
    | Trade trade -> Some (trade.recv_ts, trade.price)
    | Bbo bbo -> Some (bbo.recv_ts, Market_data.Bbo.mid bbo)
    | L2 _ | Funding _ -> None
  ;;

  let load_norm ?(resolution = Time_ns.Span.second) ~factor ~dir instrument ~from ~to_ =
    match check_factor factor with
    | Error _ as e -> return e
    | Ok () ->
      (match%bind Price_anchor.Source.of_norm ~dir instrument ~from ~to_ with
       | Error _ as e -> return e
       | Ok pipe ->
         let bucket ts =
           Time_ns.to_int_ns_since_epoch ts / Time_ns.Span.to_int_ns resolution
         in
         (* Points in reverse; the head is replaced while it is in the same bucket. *)
         let acc = ref [] in
         let%map () =
           Pipe.iter_without_pushback pipe ~f:(function
             | Gap _ | Price _ -> ()
             | Event e ->
               Option.iter (event_price e) ~f:(fun (ts, px) ->
                 if Market_data.Instrument.equal
                      (Market_data.Event.instrument e)
                      instrument
                 then (
                   let point = ts, scale_price px ~factor in
                   match !acc with
                   | (last, _) :: rest when bucket last = bucket ts ->
                     acc := point :: rest
                   | points -> acc := point :: points)))
         in
         series (List.rev !acc)
         |> Or_error.tag_s
              ~tag:
                [%message
                  "no trade or BBO prices"
                    (instrument : Market_data.Instrument.t)
                    (from : Date.t)
                    (to_ : Date.t)])
  ;;

  let ns_per_day = Time_ns.Span.to_int_ns Time_ns.Span.day

  let price_at t ~elapsed =
    match t with
    | Fixed { price; drift_bps_per_day } ->
      if Float.equal drift_bps_per_day 0.
      then price
      else (
        let days =
          Float.of_int (Time_ns.Span.to_int_ns elapsed) /. Float.of_int ns_per_day
        in
        scale_price price ~factor:((1. +. (drift_bps_per_day /. 10_000.)) **. days))
    | Series points ->
      let first, _ = points.(0) in
      let at = Time_ns.add first elapsed in
      (match
         Array.binary_search
           points
           ~compare:(fun (time, _) at -> Time_ns.compare time at)
           `Last_less_than_or_equal_to
           at
       with
       | Some i -> snd points.(i)
       | None -> snd points.(0))
  ;;
end

module State = struct
  type t =
    | Waiting
    | Following
  [@@deriving sexp, compare, equal]
end

type t =
  { target : Target.t
  ; half_life : Time_ns.Span.t
  ; mutable started_at : Time_ns.t option
  ; mutable state : State.t
  ; mutable last_set : Price.t option
  ; mutable resumes : int
  ; mutable waits : int
  }

let create ?(half_life = Time_ns.Span.second) target =
  if Time_ns.Span.( <= ) half_life Time_ns.Span.zero
  then raise_s [%message "Anchoring.create: half-life must be positive"];
  { target
  ; half_life
  ; started_at = None
  ; state = Waiting
  ; last_set = None
  ; resumes = 0
  ; waits = 0
  }
;;

let target t = t.target
let state t = t.state
let last_set t = t.last_set
let resumes t = t.resumes
let waits t = t.waits

let update t ~now ~follow ~sim ~symbol =
  let started_at =
    match t.started_at with
    | Some at -> at
    | None ->
      t.started_at <- Some now;
      now
  in
  match follow, t.state with
  | true, _ ->
    let px = Target.price_at t.target ~elapsed:(Time_ns.diff now started_at) in
    let resuming = State.equal t.state Waiting in
    if resuming || not ([%equal: Price.t option] t.last_set (Some px))
    then (
      Market_sim.set_reference ~half_life:t.half_life sim symbol px;
      t.last_set <- Some px);
    if resuming
    then (
      t.state <- Following;
      t.resumes <- t.resumes + 1)
  | false, Following ->
    Market_sim.clear_reference sim symbol;
    t.state <- Waiting;
    t.last_set <- None;
    t.waits <- t.waits + 1
  | false, Waiting -> ()
;;
