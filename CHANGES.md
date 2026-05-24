# Changes

## 0.1.0 - 2026-05-24

Initial release.

- Define endpoint contracts as typed OCaml values.
- Parse and match simple path templates such as `/users/:id`.
- Decode scalar path and query parameters.
- Percent-decode matched path parameters.
- Decode JSON request bodies with manual codecs.
- Validate pure HTTP-like request values against endpoints.
- Emit OpenAPI 3.0.3 JSON for small APIs.
- Include a users API example and focused Alcotest coverage.

Current scope is intentionally small. There are no HTTP adapters, typed clients,
mock servers, OpenAPI import, or response validation yet.
