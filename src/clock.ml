open! Core
open Equities_core

type kind =
  | Real
  | Accelerated of
      { origin_real : Time_ns.t
      ; origin_virtual : Time_ns.t
      ; speed : float
      }
  | Always_open
  | Manual of { mutable now : Time_ns.t }

type t = kind

let real () = Real

let accelerated ~start ~speed =
  Accelerated { origin_real = Time_ns.now (); origin_virtual = start; speed }
;;

let always_open () = Always_open
let manual now = Manual { now }

let set t now =
  match t with
  | Manual m -> m.now <- now
  | Real | Accelerated _ | Always_open ->
    raise_s [%message "Clock.set: not a manual clock"]
;;

let now = function
  | Real | Always_open -> Time_ns.now ()
  | Manual { now } -> now
  | Accelerated { origin_real; origin_virtual; speed } ->
    let elapsed = Time_ns.diff (Time_ns.now ()) origin_real in
    Time_ns.add origin_virtual (Time_ns.Span.scale elapsed speed)
;;

let phase t : Session.Us_equities.Phase.t =
  match t with
  | Always_open -> Core
  | Real | Accelerated _ | Manual _ -> Session.Us_equities.phase_at (now t)
;;

let has_sessions = function
  | Always_open -> false
  | Real | Accelerated _ | Manual _ -> true
;;

let local t = Session.Zone.to_local New_york (now t)

let ny_time date hms =
  Session.Zone.of_local
    New_york
    date
    (Time_ns.Ofday.to_span_since_start_of_day (Time_ns.Ofday.of_string hms))
;;
