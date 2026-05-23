type meth =
  | GET
  | POST
  | PUT
  | PATCH
  | DELETE

type packed_param =
  | Path_param : string * 'a Codec.t -> packed_param
  | Query_param : string * bool * 'a Codec.t -> packed_param

type body = Body : 'a Json_codec.t -> body
type response = Response : int * 'a Json_codec.t option -> response

type t = {
  meth : meth;
  path : Path_template.t;
  summary : string option;
  operation_id : string option;
  params : packed_param list;
  body : body option;
  responses : response list;
}

val make :
  ?summary:string ->
  ?operation_id:string ->
  meth ->
  string ->
  t

val get : ?summary:string -> ?operation_id:string -> string -> t
val post : ?summary:string -> ?operation_id:string -> string -> t
val put : ?summary:string -> ?operation_id:string -> string -> t
val patch : ?summary:string -> ?operation_id:string -> string -> t
val delete : ?summary:string -> ?operation_id:string -> string -> t

val path_param : string -> 'a Codec.t -> t -> t

val query_param :
  ?required:bool ->
  string ->
  'a Codec.t ->
  t ->
  t

val body : 'a Json_codec.t -> t -> t
val response : status:int -> 'a Json_codec.t -> t -> t
val empty_response : status:int -> t -> t
val meth_to_string : meth -> string
val meth_to_openapi_key : meth -> string
