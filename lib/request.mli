type t = {
  method_ : Endpoint.method_;
  path : string;
  query : (string * string) list;
  body : Yojson.Safe.t option;
}
(** Pure HTTP-like request value used by the request validator.

    Query values are already split into key/value pairs. The library does not
    parse URLs; path template matching percent-decodes captured path parameters.
*)

val make :
  ?query:(string * string) list ->
  ?body:Yojson.Safe.t ->
  method_:Endpoint.method_ ->
  path:string ->
  unit ->
  t
