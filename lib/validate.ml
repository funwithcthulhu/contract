type validated = {
  endpoint : Endpoint.t;
  path_values : (string * string) list;
  query_values : (string * string) list;
  body : Yojson.Safe.t option;
}

type validated_response = {
  endpoint : Endpoint.t;
  status : int;
  body : Yojson.Safe.t option;
}

type response_body = No_body | Json_body : 'a Json_codec.t -> response_body

let retag location = function
  | Ok value -> Ok value
  | Error error -> Error { error with Error.location }

let decode_scalar location codec value =
  codec.Codec.decode value |> retag location

let required_query_missing name =
  Error.make ~location:(Error.Query_param name)
    "missing required query parameter"

let path_param_missing name =
  Error.make ~location:(Error.Path_param name) "missing path parameter"

let first_value name values = List.assoc_opt name values

let validate_param path_values query_values = function
  | Endpoint.Path_param (name, codec) -> (
      match first_value name path_values with
      | None -> Some (path_param_missing name)
      | Some value -> (
          match decode_scalar (Error.Path_param name) codec value with
          | Ok _ -> None
          | Error error -> Some error))
  | Endpoint.Query_param (name, required, codec) -> (
      match first_value name query_values with
      | None when required -> Some (required_query_missing name)
      | None -> None
      | Some value -> (
          match decode_scalar (Error.Query_param name) codec value with
          | Ok _ -> None
          | Error error -> Some error))

let validate_body endpoint_body request_body =
  match (endpoint_body, request_body) with
  | None, _ -> None
  | Some _, None ->
      Some (Error.make ~location:Error.Body "missing request body")
  | Some (Endpoint.Body codec), Some json -> (
      match codec.Json_codec.decode json with
      | Ok _ -> None
      | Error error -> Some error)

let status_to_string status = string_of_int status

let response_statuses endpoint =
  List.map
    (fun (Endpoint.Response (status, _)) -> status_to_string status)
    endpoint.Endpoint.responses

let response_for_status endpoint status =
  List.find_map
    (fun (Endpoint.Response (declared_status, body)) ->
      if declared_status = status then
        Some (match body with None -> No_body | Some codec -> Json_body codec)
      else None)
    endpoint.Endpoint.responses

let unexpected_status endpoint response =
  let expected =
    match response_statuses endpoint with
    | [] -> None
    | statuses -> Some (String.concat ", " statuses)
  in
  Error.make ?expected
    ~got:(status_to_string response.Response.status)
    ~location:Error.Status "unexpected response status"

let validate_response_body expected_body response_body =
  match (expected_body, response_body) with
  | No_body, None -> None
  | No_body, Some _ ->
      Some (Error.make ~location:Error.Body "unexpected response body")
  | Json_body _, None ->
      Some (Error.make ~location:Error.Body "missing response body")
  | Json_body codec, Some json -> (
      match codec.Json_codec.decode json with
      | Ok _ -> None
      | Error error -> Some error)

let request endpoint request =
  if endpoint.Endpoint.method_ <> request.Request.method_ then
    Error
      [
        Error.make ~location:Error.Method
          ~expected:(Endpoint.method_to_string endpoint.method_)
          ~got:(Endpoint.method_to_string request.method_)
          "HTTP method does not match";
      ]
  else
    match Path_template.match_path endpoint.path request.path with
    | Error error -> Error [ error ]
    | Ok path_values ->
        let param_errors =
          endpoint.params
          |> List.filter_map (validate_param path_values request.query)
        in
        let body_errors =
          match validate_body endpoint.body request.body with
          | None -> []
          | Some error -> [ error ]
        in
        let errors = param_errors @ body_errors in
        if errors = [] then
          Ok
            {
              endpoint;
              path_values;
              query_values = request.query;
              body = request.body;
            }
        else Error errors

let path validated name codec =
  match first_value name validated.path_values with
  | None -> Error (path_param_missing name)
  | Some value -> decode_scalar (Error.Path_param name) codec value

let query validated name codec =
  match first_value name validated.query_values with
  | None -> Ok None
  | Some value -> (
      match decode_scalar (Error.Query_param name) codec value with
      | Ok value -> Ok (Some value)
      | Error error -> Error error)

let decode_optional_json body codec =
  match body with
  | None -> Ok None
  | Some json -> (
      match codec.Json_codec.decode json with
      | Ok value -> Ok (Some value)
      | Error error -> Error error)

let body (validated : validated) codec =
  decode_optional_json validated.body codec

let response_body (validated : validated_response) codec =
  decode_optional_json validated.body codec

let response endpoint (response : Response.t) =
  match response_for_status endpoint response.status with
  | None -> Error [ unexpected_status endpoint response ]
  | Some expected_body -> (
      match validate_response_body expected_body response.body with
      | None -> Ok { endpoint; status = response.status; body = response.body }
      | Some error -> Error [ error ])
