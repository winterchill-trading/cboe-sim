open! Core
open Equities_core
module Price = Exchange_core.Price

module Kind = struct
  type t =
    | Equity of { tier : Halts.Tier.t }
    | Option of
        { osi : Symbol.Osi.t
        ; option_class : Tick_size.Option_class.t
        }
  [@@deriving sexp_of]
end

type t =
  { symbol : string
  ; kind : Kind.t
  ; unit : int
  ; reference_price : Price.t
  }
[@@deriving sexp_of]

let equities_unit = 1
let options_unit = 2

let equity ?(tier = Halts.Tier.Tier1) symbol px =
  { symbol
  ; kind = Equity { tier }
  ; unit = equities_unit
  ; reference_price = Price.of_string px
  }
;;

let default_equities =
  [ equity "AAPL" "230.00"
  ; equity "MSFT" "420.00"
  ; equity "NVDA" "180.00"
  ; equity "AMZN" "220.00"
  ; equity "SPY" "600.00"
  ; equity ~tier:Tier2 "ZVZZT" "10.00"
  ]
;;

let spy_options =
  let expiry = Date.create_exn ~y:2026 ~m:Dec ~d:18 in
  [ 590, Symbol.Osi.Right.Call, "25.00"
  ; 600, Call, "18.00"
  ; 610, Call, "12.00"
  ; 590, Put, "9.00"
  ; 600, Put, "13.00"
  ; 610, Put, "18.00"
  ]
  |> List.mapi ~f:(fun i (strike, right, px) ->
    { symbol = sprintf "SPY%03d" (i + 1)
    ; kind =
        Option
          { osi = { root = "SPY"; expiry; right; strike = Price.of_int strike }
          ; option_class = Penny_all
          }
    ; unit = options_unit
    ; reference_price = Price.of_string px
    })
;;

let universe ?symbols ?(options = true) () =
  let equities =
    match symbols with
    | None -> default_equities
    | Some symbols ->
      List.map symbols ~f:(fun s ->
        match List.find default_equities ~f:(fun i -> String.equal i.symbol s) with
        | Some i -> i
        | None -> raise_s [%message "unknown symbol" s])
  in
  equities @ if options then spy_options else []
;;

let is_option t =
  match t.kind with
  | Option _ -> true
  | Equity _ -> false
;;

let to_engine t =
  Exchange_core.Instrument.create
    ~fees:(Fees.to_engine Fees.bzx_like ~price:t.reference_price)
    (Exchange_core.Symbol.of_string t.symbol)
    ~size_decimals:0
    ~price_tick:(Price.of_string "0.01")
    ~max_leverage:4
;;

let find ts symbol = List.find ts ~f:(fun t -> String.equal t.symbol symbol)
let to_price4 p = Price.to_units p / 10_000
let of_price4 p = Price.of_units (p * 10_000)

let find_option ts ~root ~maturity ~strike ~put_or_call =
  List.find ts ~f:(fun t ->
    match t.kind with
    | Equity _ -> false
    | Option { osi; _ } ->
      String.equal osi.root root
      && Date.to_string_iso8601_basic osi.expiry |> Int.of_string = maturity
      && to_price4 osi.strike = strike
      &&
        (match osi.right, put_or_call with
        | Call, '1' | Put, '0' -> true
        | _ -> false))
;;

let is_valid_price t px =
  match t.kind with
  | Equity _ -> Tick_size.is_valid_equity_price px
  | Option { option_class; _ } -> Tick_size.is_valid_option_price option_class px
;;
