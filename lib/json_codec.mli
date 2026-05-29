(** Manual JSON codecs for request bodies and schemas.

    These codecs are deliberately direct. Record codecs should usually be
    written by hand with [required_field] and [optional_field]. *)

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
(** [option codec] treats JSON [null] as [None]. *)

val list : 'a t -> 'a list t
(** Decodes every list item with the supplied codec and stops at the first
    error. *)

val required_field : string -> 'a t -> Yojson.Safe.t -> ('a, Error.t) result
(** Decode a required field from a JSON object. Missing fields and non-object
    values return [Error]. Unrelated object fields are ignored. *)

val optional_field :
  string -> 'a t -> Yojson.Safe.t -> ('a option, Error.t) result
(** Decode an optional field from a JSON object. Missing fields and JSON [null]
    both return [Ok None]. Unrelated object fields are ignored. *)
