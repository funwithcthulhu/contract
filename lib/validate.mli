(** Result of matching a pure request against one endpoint. *)

type validated = {
  endpoint : Endpoint.t;
      (** Percent-decoded values captured from the matched path template. *)
  path_values : (string * string) list;
  query_values : (string * string) list;
  body : Yojson.Safe.t option;
}

type validated_response = {
  endpoint : Endpoint.t;
  status : int;
  body : Yojson.Safe.t option;
}

val request : Endpoint.t -> Request.t -> (validated, Error.t list) result
(** Validate method, route, declared parameters, and request body.

    Undeclared query parameters and request bodies on body-less endpoints are
    ignored. If a query parameter appears more than once, validation and
    decoding use the first matching value. Declared path parameters must be
    present in the matched template values; declaring a path parameter that is
    not present in the template is a validation error. Body field strictness is
    controlled by the JSON codec. *)

val path : validated -> string -> 'a Codec.t -> ('a, Error.t) result
(** Decode a matched path parameter from a validated request. *)

val query : validated -> string -> 'a Codec.t -> ('a option, Error.t) result
(** Decode a query parameter from a validated request. Missing parameters return
    [Ok None]. Duplicate parameters use the first matching value. *)

val body : validated -> 'a Json_codec.t -> ('a option, Error.t) result
(** Decode the JSON body from a validated request. Missing bodies return
    [Ok None]. *)

val response :
  Endpoint.t -> Response.t -> (validated_response, Error.t list) result
(** Validate a pure response against the endpoint's declared responses.

    The status must match a declared response. If that response declares a JSON
    body, the body must be present and decode with its codec. If it declares an
    empty response, a present body is rejected. Duplicate response status
    declarations use the first matching declaration. *)

val response_body :
  validated_response -> 'a Json_codec.t -> ('a option, Error.t) result
(** Decode the JSON body from a validated response. Missing bodies return
    [Ok None]. The caller supplies the codec because successful response
    validation preserves the response as JSON, not as an existential decoded
    value. *)
