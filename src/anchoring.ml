open! Core
open! Async
open! Exchange_core
module Anchor = Price_anchor.Anchor
module Path = Price_anchor.Path
module Source = Price_anchor.Source
module Exchange = Exchange_core_engine.Exchange

let check_factor factor =
  if Float.is_finite factor && Float.( > ) factor 0.
  then Ok ()
  else Or_error.error_s [%message "price factor must be positive" (factor : float)]
;;

module Target = struct
  type t =
    { path : Path.t
    ; price_factor : float
    ; reference : Market_data.Instrument.t option
    }
  [@@deriving sexp_of]

  let of_path ?reference ?(price_factor = 1.) path = { path; price_factor; reference }
  let series points = Path.series points |> Or_error.map ~f:of_path

  let fixed ?drift_bps_per_day price =
    Path.fixed ?drift_bps_per_day price |> Or_error.map ~f:of_path
  ;;

  let parse_fixed s = Path.fixed_of_string s |> Or_error.map ~f:of_path

  let load_source ?reference ?min_interval ~factor source =
    match check_factor factor with
    | Error _ as e -> return e
    | Ok () ->
      Deferred.Or_error.bind source ~f:(fun source ->
        Source.to_path ?min_interval source
        |> Deferred.Or_error.map ~f:(of_path ?reference ~price_factor:factor))
  ;;

  let load_series ?(factor = 1.) path = load_source ~factor (Source.of_series path)

  let load_norm ?(resolution = Time_ns.Span.second) ~factor ~dir instrument ~from ~to_ =
    load_source
      ~reference:instrument
      ~min_interval:resolution
      ~factor
      (Source.of_norm ~dir instrument ~from ~to_)
  ;;

  let price_at t ~elapsed =
    let px = Path.price_at t.path ~elapsed in
    if Float.equal t.price_factor 1.
    then px
    else
      Price.of_units
        (Float.iround_nearest_exn (Float.of_int (Price.to_units px) *. t.price_factor))
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
  ; anchor : Anchor.t
  ; exchange : Exchange.t
  }

let create
  ?(half_life = Time_ns.Span.second)
  ~noise_traders
  ~may_engage
  ~last_trade
  ~clamp
  ~market_sim
  ~exchange
  ~symbol
  (target : Target.t)
  =
  let reference =
    Option.value_or_thunk target.reference ~default:(fun () ->
      Market_data.Instrument.create Cboe (Symbol.to_string symbol))
  in
  let anchor =
    Anchor.create
      ~price_factor:target.price_factor
      ~levels:0
      ~mark_interval:Time_ns.Span.zero
      ~noise_traders
      ~half_life
      ~may_engage
      ~last_trade
      ~step_clock:false
      ~clamp
      ~path:target.path
      ~market_sim
      ~exchange
      ~reference
      ~sim_symbol:symbol
      ()
  in
  { target; anchor; exchange }
;;

let target t = t.target
let last_set t = Anchor.last_set t.anchor

let state t =
  if Option.is_some (last_set t) && not (Anchor.is_paused t.anchor)
  then State.Following
  else Waiting
;;

let stats t = Anchor.stats t.anchor
let resumes t = (stats t).resumes
let waits t = (stats t).pauses
let step t = Anchor.step t.anchor ~now:(Exchange.now t.exchange)
