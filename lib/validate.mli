(** Result of matching a pure request against one endpoint. *)

type validated = {
  endpoint : Endpoint.t;
  path_values : (string * string) list;
  query_values : (string * string) list;
  body : Yojson.Safe.t option;
}

(** Validate method, route, declared parameters, and request body.

    Undeclared query parameters and request bodies on body-less endpoints are
    ignored for now. *)
val request :
  Endpoint.t ->
  Request.t ->
  (validated, Error.t list) result

(** Decode a matched path parameter from a validated request. *)
val path :
  validated ->
  string ->
  'a Codec.t ->
  ('a, Error.t) result

(** Decode a query parameter from a validated request. Missing parameters return
    [Ok None]. *)
val query :
  validated ->
  string ->
  'a Codec.t ->
  ('a option, Error.t) result

(** Decode the JSON body from a validated request. Missing bodies return
    [Ok None]. *)
val body :
  validated ->
  'a Json_codec.t ->
  ('a option, Error.t) result
