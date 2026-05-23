# contract

`contract` is an OCaml library for describing HTTP API contracts as typed values. The current code covers a small pure core: endpoint definitions, path matching, scalar and JSON decoding, request validation, and OpenAPI output.

## Current MVP

This version is a thin vertical slice for REST-style JSON APIs. It has no HTTP server dependency. A request is just a value passed to the validator.

See `examples/users_api.ml` for a small users API with `GET /users/:id` and `POST /users`.

```sh
dune exec examples/users_api.exe
```

Build and test:

```sh
dune build @all
dune runtest
```

Current limitations:

- no real HTTP adapter yet
- no typed client yet
- no OpenAPI import yet
- no mock server yet
- no response validation yet
- no percent-decoding yet

Short roadmap:

- stabilize core API
- refine public signatures
- add Dream adapter
- add Eio/Cohttp client
- add mock server
- add contract tests
- improve OpenAPI support
