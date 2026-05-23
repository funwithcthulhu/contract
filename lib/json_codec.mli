type 'a t = {
  name : string option;
  schema : Schema.t;
  encode : 'a -> Yojson.Safe.t;
  decode : Yojson.Safe.t -> ('a, Error.t) result;
}

val make :
  ?name:string ->
  schema:Schema.t ->
  encode:('a -> Yojson.Safe.t) ->
  decode:(Yojson.Safe.t -> ('a, Error.t) result) ->
  unit ->
  'a t

val string : string t
val int : int t
val bool : bool t
val float : float t
val option : 'a t -> 'a option t
val list : 'a t -> 'a list t

val required_field :
  string ->
  'a t ->
  Yojson.Safe.t ->
  ('a, Error.t) result

val optional_field :
  string ->
  'a t ->
  Yojson.Safe.t ->
  ('a option, Error.t) result
