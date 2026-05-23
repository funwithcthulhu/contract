type t = {
  meth : Endpoint.meth;
  path : string;
  query : (string * string) list;
  body : Yojson.Safe.t option;
}

val make :
  ?query:(string * string) list ->
  ?body:Yojson.Safe.t ->
  meth:Endpoint.meth ->
  path:string ->
  unit ->
  t
