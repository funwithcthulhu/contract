(lang dune 3.11)

(name contract)
(version 0.2.0)

(package
 (name contract)
 (synopsis "Typed HTTP API contracts for OCaml")
 (description
  "A pure core for typed HTTP API contracts, request and response validation, and OpenAPI output.")
 (license MIT)
 (depends
  (ocaml (>= 5.0))
  (dune (>= 3.11))
  (yojson (>= 2.0.0))
  (alcotest :with-test)))
