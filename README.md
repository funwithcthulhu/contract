# contract

[![CI](https://github.com/funwithcthulhu/contract/actions/workflows/ci.yml/badge.svg)](https://github.com/funwithcthulhu/contract/actions)
[![opam](https://img.shields.io/opam/v/contract.svg)](https://opam.ocaml.org/packages/contract/)
[![license: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

`contract` is an OCaml library for describing HTTP API contracts as typed values. The current code covers a small pure core: endpoint definitions, path matching, scalar and JSON decoding, request validation, and OpenAPI output.

## Current MVP

This version is a thin vertical slice for REST-style JSON APIs. It has no HTTP server dependency. A request is just a value passed to the validator.
Path parameters are percent-decoded after route matching.

Install:

```sh
opam install contract
```

See `examples/users_api.ml` for a small users API with `GET /users/:id` and `POST /users`.

```sh
dune exec examples/users_api.exe
```

Development:

```sh
dune fmt
dune build @all
dune runtest
```

Current limitations:

- no real HTTP adapter yet
- no typed client yet
- no OpenAPI import yet
- no mock server yet
- no response validation yet
