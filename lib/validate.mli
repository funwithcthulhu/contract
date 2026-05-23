type validated = {
  endpoint : Endpoint.t;
  path_values : (string * string) list;
  query_values : (string * string) list;
  body : Yojson.Safe.t option;
}

val request :
  Endpoint.t ->
  Request.t ->
  (validated, Error.t list) result

val path :
  validated ->
  string ->
  'a Codec.t ->
  ('a, Error.t) result

val query :
  validated ->
  string ->
  'a Codec.t ->
  ('a option, Error.t) result

val body :
  validated ->
  'a Json_codec.t ->
  ('a option, Error.t) result
