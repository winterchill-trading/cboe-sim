(lang dune 3.17)

(name cboe-sim)

(generate_opam_files true)

(license MIT)

(maintainers "winterchill-trading")

(authors "winterchill-trading")

(source
 (github winterchill-trading/cboe-sim))

(package
 (name cboe-sim)
 (synopsis
  "Cboe US equities and options simulator speaking BOE v2 and PITCH over TCP")
 (depends
  (ocaml
   (>= 5.2))
  dune
  core
  core_unix
  async
  cohttp
  cohttp-async
  http
  async_websocket
  uri
  yojson
  fixed-decimal
  exchange-core
  exchange-core-engine
  order-book
  equities-core
  market-data
  price-anchor
  ppx_jane))
