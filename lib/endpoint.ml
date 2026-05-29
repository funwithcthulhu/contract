type method_ = GET | POST | PUT | PATCH | DELETE

type param =
  | Path_param : string * 'a Codec.t -> param
  | Query_param : string * bool * 'a Codec.t -> param

type body = Body : 'a Json_codec.t -> body
type response = Response : int * 'a Json_codec.t option -> response

type t = {
  method_ : method_;
  path : Path_template.t;
  summary : string option;
  operation_id : string option;
  params : param list;
  body : body option;
  responses : response list;
}

let make ?summary ?operation_id method_ path =
  match Path_template.parse path with
  | Error error -> Error error
  | Ok path ->
      Ok
        {
          method_;
          path;
          summary;
          operation_id;
          params = [];
          body = None;
          responses = [];
        }

let get ?summary ?operation_id path = make ?summary ?operation_id GET path
let post ?summary ?operation_id path = make ?summary ?operation_id POST path
let put ?summary ?operation_id path = make ?summary ?operation_id PUT path
let patch ?summary ?operation_id path = make ?summary ?operation_id PATCH path
let delete ?summary ?operation_id path = make ?summary ?operation_id DELETE path

let path_param name codec endpoint =
  { endpoint with params = endpoint.params @ [ Path_param (name, codec) ] }

let query_param ?(required = false) name codec endpoint =
  {
    endpoint with
    params = endpoint.params @ [ Query_param (name, required, codec) ];
  }

let body codec endpoint = { endpoint with body = Some (Body codec) }

let response ~status codec endpoint =
  {
    endpoint with
    responses = endpoint.responses @ [ Response (status, Some codec) ];
  }

let empty_response ~status endpoint =
  { endpoint with responses = endpoint.responses @ [ Response (status, None) ] }

let method_to_string = function
  | GET -> "GET"
  | POST -> "POST"
  | PUT -> "PUT"
  | PATCH -> "PATCH"
  | DELETE -> "DELETE"

let method_to_openapi_key = function
  | GET -> "get"
  | POST -> "post"
  | PUT -> "put"
  | PATCH -> "patch"
  | DELETE -> "delete"
