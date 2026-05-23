(** Pure HTTP-like request value used by the validator.

    Query values are already split into key/value pairs. The library does not
    parse URLs or perform percent-decoding yet. *)
type t = {
  method_ : Endpoint.method_;
  path : string;
  query : (string * string) list;
  body : Yojson.Safe.t option;
}

val make :
  ?query:(string * string) list ->
  ?body:Yojson.Safe.t ->
  method_:Endpoint.method_ ->
  path:string ->
  unit ->
  t
