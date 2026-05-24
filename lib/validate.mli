(** Result of matching a pure request against one endpoint. *)

type validated = {
  endpoint : Endpoint.t;
  (** Percent-decoded values captured from the matched path template. *)
  path_values : (string * string) list;
  query_values : (string * string) list;
  body : Yojson.Safe.t option;
}

(** Validate method, route, declared parameters, and request body.

    Undeclared query parameters and request bodies on body-less endpoints are
    ignored. If a query parameter appears more than once, validation and
    decoding use the first matching value. Declared path parameters must be
    present in the matched template values; declaring a path parameter that is
    not present in the template is a validation error. Body field strictness is
    controlled by the JSON codec. *)
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
    [Ok None]. Duplicate parameters use the first matching value. *)
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
