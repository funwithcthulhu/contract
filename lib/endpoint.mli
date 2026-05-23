(** HTTP method names supported by the core contract model. *)
type method_ =
  | GET
  | POST
  | PUT
  | PATCH
  | DELETE

(** A declared path or query parameter.

    Query parameters carry their required flag. Path parameters are always
    required by the route template. *)
type param =
  | Path_param : string * 'a Codec.t -> param
  | Query_param : string * bool * 'a Codec.t -> param

type body = Body : 'a Json_codec.t -> body
type response = Response : int * 'a Json_codec.t option -> response

(** Endpoint contract values are transparent for now so callers can inspect and
    emit them. Prefer the constructors below over hand-built records; they
    validate path templates. *)
type t = {
  method_ : method_;
  path : Path_template.t;
  summary : string option;
  operation_id : string option;
  params : param list;
  body : body option;
  responses : response list;
}

val make :
  ?summary:string ->
  ?operation_id:string ->
  method_ ->
  string ->
  (t, Error.t) result

(** Method-specific constructors. They validate the path template and return
    [Error] instead of raising for malformed templates. *)
val get :
  ?summary:string ->
  ?operation_id:string ->
  string ->
  (t, Error.t) result

val post :
  ?summary:string ->
  ?operation_id:string ->
  string ->
  (t, Error.t) result

val put :
  ?summary:string ->
  ?operation_id:string ->
  string ->
  (t, Error.t) result

val patch :
  ?summary:string ->
  ?operation_id:string ->
  string ->
  (t, Error.t) result

val delete :
  ?summary:string ->
  ?operation_id:string ->
  string ->
  (t, Error.t) result

(** Add a declared path parameter. The name should correspond to a [:name]
    segment in the path template; validation reports an error if it does not. *)
val path_param : string -> 'a Codec.t -> t -> t

(** Add a declared query parameter. Query parameters are optional unless
    [~required:true] is supplied. *)
val query_param :
  ?required:bool ->
  string ->
  'a Codec.t ->
  t ->
  t

(** Declare a JSON request body. *)
val body : 'a Json_codec.t -> t -> t

(** Declare a JSON response body for [status]. Response bodies are not validated
    by the current core validator. *)
val response : status:int -> 'a Json_codec.t -> t -> t

val empty_response : status:int -> t -> t
val method_to_string : method_ -> string
val method_to_openapi_key : method_ -> string
