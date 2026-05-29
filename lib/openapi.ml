type api = { title : string; version : string; endpoints : Endpoint.t list }

let optional name = function
  | None -> []
  | Some value -> [ (name, `String value) ]

let parameter_to_yojson = function
  | Endpoint.Path_param (name, codec) ->
      `Assoc
        [
          ("name", `String name);
          ("in", `String "path");
          ("required", `Bool true);
          ("schema", Schema.to_openapi codec.schema);
        ]
  | Endpoint.Query_param (name, required, codec) ->
      `Assoc
        [
          ("name", `String name);
          ("in", `String "query");
          ("required", `Bool required);
          ("schema", Schema.to_openapi codec.schema);
        ]

let json_content schema =
  `Assoc
    [ ("application/json", `Assoc [ ("schema", Schema.to_openapi schema) ]) ]

let request_body_to_yojson = function
  | Endpoint.Body codec ->
      `Assoc
        [ ("required", `Bool true); ("content", json_content codec.schema) ]

let response_description status =
  if status >= 200 && status < 300 then "Success" else "Response"

let response_to_pair (Endpoint.Response (status, codec)) =
  let fields =
    [ ("description", `String (response_description status)) ]
    @
    match codec with
    | None -> []
    | Some codec -> [ ("content", json_content codec.schema) ]
  in
  (string_of_int status, `Assoc fields)

let operation_to_yojson endpoint =
  let fields =
    optional "summary" endpoint.Endpoint.summary
    @ optional "operationId" endpoint.operation_id
    @ [ ("parameters", `List (List.map parameter_to_yojson endpoint.params)) ]
    @ (match endpoint.body with
      | None -> []
      | Some body -> [ ("requestBody", request_body_to_yojson body) ])
    @ [ ("responses", `Assoc (List.map response_to_pair endpoint.responses)) ]
  in
  `Assoc fields

let add_endpoint paths endpoint =
  let path = Path_template.to_openapi_path endpoint.Endpoint.path in
  let method_ = Endpoint.method_to_openapi_key endpoint.method_ in
  let operation = operation_to_yojson endpoint in
  match List.assoc_opt path paths with
  | None -> paths @ [ (path, `Assoc [ (method_, operation) ]) ]
  | Some (`Assoc methods_) ->
      let paths_without_current =
        List.filter
          (fun (candidate, _) -> not (String.equal candidate path))
          paths
      in
      paths_without_current
      @ [ (path, `Assoc (methods_ @ [ (method_, operation) ])) ]
  | Some _ -> paths

let to_yojson api =
  let paths = List.fold_left add_endpoint [] api.endpoints in
  `Assoc
    [
      ("openapi", `String "3.0.3");
      ( "info",
        `Assoc
          [ ("title", `String api.title); ("version", `String api.version) ] );
      ("paths", `Assoc paths);
    ]

let to_string ?(pretty = false) api =
  let json = to_yojson api in
  if pretty then Yojson.Safe.pretty_to_string json
  else Yojson.Safe.to_string json
