# Changes

## 0.2.0 - 2026-05-29

- Add pure response validation for declared status codes and JSON response
  bodies.
- Add a helper for decoding validated response bodies.
- Tighten response-validation tests for declared statuses and response body
  errors.

## 0.1.0 - 2026-05-24

Initial release of the pure OCaml contract core.

- Add endpoint definitions, path matching, request validation, scalar and JSON
  decoding, and OpenAPI output.
- Add percent-decoding for captured path parameters.
- Include a users API example and focused Alcotest coverage.

Current scope is intentionally small. There are no HTTP adapters, typed clients,
mock servers, or OpenAPI import yet.
