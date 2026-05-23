type validated = {
  endpoint : Endpoint.t;
  path_values : (string * string) list;
  query_values : (string * string) list;
  body : Yojson.Safe.t option;
}

let retag location = function
  | Ok value -> Ok value
  | Error error -> Error { error with Error.location = location }

let decode_scalar location codec value =
  codec.Codec.decode value |> retag location

let required_query_missing name =
  Error.make ~location:(Error.Query_param name) "missing required query parameter"

let path_param_missing name =
  Error.make ~location:(Error.Path_param name) "missing path parameter"

let first_value name values =
  List.assoc_opt name values

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
  | Some _, None -> Some (Error.make ~location:Error.Body "missing request body")
  | Some (Endpoint.Body codec), Some json -> (
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

let body validated codec =
  match validated.body with
  | None -> Ok None
  | Some json -> (
      match codec.Json_codec.decode json with
      | Ok value -> Ok (Some value)
      | Error error -> Error error)
