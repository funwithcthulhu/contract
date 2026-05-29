open Contract

let user_codec =
  let schema =
    Schema.obj [ ("id", Schema.integer, true); ("email", Schema.string, true) ]
  in
  Json_codec.make ~name:"User" ~schema
    ~encode:(fun () -> `Assoc [])
    ~decode:(fun _ -> Ok ())
    ()

let create_user_codec =
  let schema = Schema.obj [ ("email", Schema.string, true) ] in
  Json_codec.make ~name:"CreateUser" ~schema
    ~encode:(fun () -> `Assoc [])
    ~decode:(fun _ -> Ok ())
    ()

let expect_endpoint = function
  | Ok endpoint -> endpoint
  | Error error -> Alcotest.fail (Error.to_string error)

let get_user =
  Endpoint.get ~summary:"Fetch a user" ~operation_id:"getUser" "/users/:id"
  |> Result.map (Endpoint.path_param "id" Codec.int)
  |> Result.map (Endpoint.query_param "include_deleted" Codec.bool)
  |> Result.map (Endpoint.response ~status:200 user_codec)
  |> expect_endpoint

let post_user =
  Endpoint.post ~summary:"Create a user" ~operation_id:"createUser" "/users"
  |> Result.map (Endpoint.body create_user_codec)
  |> Result.map (Endpoint.response ~status:201 user_codec)
  |> expect_endpoint

let list_sessions =
  Endpoint.get ~summary:"List sessions" "/sessions"
  |> Result.map (Endpoint.response ~status:200 user_codec)
  |> expect_endpoint

let create_session =
  Endpoint.post ~summary:"Create session" "/sessions"
  |> Result.map (Endpoint.body create_user_codec)
  |> Result.map (Endpoint.response ~status:201 user_codec)
  |> expect_endpoint

let api : Openapi.api =
  {
    title = "Users API";
    version = "0.2.0";
    endpoints = [ get_user; post_user ];
  }

let member name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let require_member name json =
  match member name json with
  | Some value -> value
  | None -> Alcotest.fail ("missing member: " ^ name)

let require_path path json =
  let paths = require_member "paths" json in
  require_member path paths

let require_string name json =
  match require_member name json with
  | `String value -> value
  | _ -> Alcotest.fail (name ^ " should be a string")

let require_bool name json =
  match require_member name json with
  | `Bool value -> value
  | _ -> Alcotest.fail (name ^ " should be a boolean")

let require_list name json =
  match require_member name json with
  | `List values -> values
  | _ -> Alcotest.fail (name ^ " should be a list")

let schema_type json = require_string "type" json

let json_schema operation =
  operation |> require_member "content"
  |> require_member "application/json"
  |> require_member "schema"

let output_contains_openapi_version () =
  let json = Openapi.to_yojson api in
  match require_member "openapi" json with
  | `String version -> Alcotest.(check string) "version" "3.0.3" version
  | _ -> Alcotest.fail "openapi should be a string"

let output_contains_users_id_path () =
  Openapi.to_yojson api |> require_path "/users/{id}" |> ignore

let output_contains_get_operation () =
  Openapi.to_yojson api |> require_path "/users/{id}" |> require_member "get"
  |> ignore

let output_contains_post_operation () =
  Openapi.to_yojson api |> require_path "/users" |> require_member "post"
  |> ignore

let same_path_get_and_post_share_one_path_item () =
  let json =
    Openapi.to_yojson
      {
        title = "Sessions API";
        version = "0.2.0";
        endpoints = [ list_sessions; create_session ];
      }
  in
  let paths = require_member "paths" json in
  let path_count =
    match paths with
    | `Assoc fields ->
        fields
        |> List.filter (fun (path, _) -> String.equal path "/sessions")
        |> List.length
    | _ -> Alcotest.fail "paths should be an object"
  in
  Alcotest.(check int) "sessions path count" 1 path_count;
  let sessions = require_member "/sessions" paths in
  require_member "get" sessions |> ignore;
  require_member "post" sessions |> ignore

let output_contains_post_request_body () =
  Openapi.to_yojson api |> require_path "/users" |> require_member "post"
  |> require_member "requestBody"
  |> ignore

let output_contains_response_200 () =
  Openapi.to_yojson api |> require_path "/users/{id}" |> require_member "get"
  |> require_member "responses" |> require_member "200" |> ignore

let output_contains_request_schema () =
  let schema =
    Openapi.to_yojson api |> require_path "/users" |> require_member "post"
    |> require_member "requestBody"
    |> json_schema
  in
  Alcotest.(check string) "schema type" "object" (schema_type schema);
  let properties = require_member "properties" schema in
  let email = require_member "email" properties in
  Alcotest.(check string) "email type" "string" (schema_type email);
  Alcotest.(check (list string))
    "required" [ "email" ]
    (schema |> require_list "required"
    |> List.map (function
      | `String value -> value
      | _ -> Alcotest.fail "required item should be a string"))

let output_contains_response_schema () =
  let schema =
    Openapi.to_yojson api |> require_path "/users/{id}" |> require_member "get"
    |> require_member "responses" |> require_member "200" |> json_schema
  in
  Alcotest.(check string) "schema type" "object" (schema_type schema);
  let properties = require_member "properties" schema in
  let id = require_member "id" properties in
  Alcotest.(check string) "id type" "integer" (schema_type id)

let parameter named parameters =
  parameters
  |> List.find_opt (fun json ->
      match member "name" json with
      | Some (`String name) -> String.equal name named
      | _ -> false)
  |> function
  | Some json -> json
  | None -> Alcotest.fail ("missing parameter: " ^ named)

let output_contains_parameters () =
  let parameters =
    Openapi.to_yojson api |> require_path "/users/{id}" |> require_member "get"
    |> require_list "parameters"
  in
  let id = parameter "id" parameters in
  Alcotest.(check string) "id in" "path" (require_string "in" id);
  Alcotest.(check bool) "id required" true (require_bool "required" id);
  Alcotest.(check string)
    "id schema" "integer"
    (id |> require_member "schema" |> schema_type);
  let include_deleted = parameter "include_deleted" parameters in
  Alcotest.(check string)
    "include_deleted in" "query"
    (require_string "in" include_deleted);
  Alcotest.(check bool)
    "include_deleted required" false
    (require_bool "required" include_deleted);
  Alcotest.(check string)
    "include_deleted schema" "boolean"
    (include_deleted |> require_member "schema" |> schema_type)

let output_is_deterministic () =
  Alcotest.(check string)
    "openapi" (Openapi.to_string api) (Openapi.to_string api)

let output_escapes_strings () =
  let endpoint =
    Endpoint.get ~summary:"Fetch \"quoted\" user" "/quoted/:id"
    |> Result.map (Endpoint.path_param "id" Codec.string)
    |> Result.map (Endpoint.response ~status:200 Json_codec.string)
    |> expect_endpoint
  in
  let json =
    Openapi.to_string
      { title = "Quoted \"API\""; version = "0.2.0"; endpoints = [ endpoint ] }
    |> Yojson.Safe.from_string
  in
  let info = require_member "info" json in
  Alcotest.(check string) "title" "Quoted \"API\"" (require_string "title" info);
  let summary =
    json
    |> require_path "/quoted/{id}"
    |> require_member "get" |> require_string "summary"
  in
  Alcotest.(check string) "summary" "Fetch \"quoted\" user" summary

let empty_api_has_empty_paths () =
  let json =
    Openapi.to_yojson { title = "Empty API"; version = "0.2.0"; endpoints = [] }
  in
  match require_member "paths" json with
  | `Assoc [] -> ()
  | _ -> Alcotest.fail "empty API should emit an empty paths object"

let tests =
  ( "openapi",
    [
      Alcotest.test_case "contains OpenAPI version" `Quick
        output_contains_openapi_version;
      Alcotest.test_case "contains /users/{id}" `Quick
        output_contains_users_id_path;
      Alcotest.test_case "contains GET operation" `Quick
        output_contains_get_operation;
      Alcotest.test_case "contains POST operation" `Quick
        output_contains_post_operation;
      Alcotest.test_case "same path GET and POST share one path item" `Quick
        same_path_get_and_post_share_one_path_item;
      Alcotest.test_case "contains POST requestBody" `Quick
        output_contains_post_request_body;
      Alcotest.test_case "contains response 200" `Quick
        output_contains_response_200;
      Alcotest.test_case "contains request schema" `Quick
        output_contains_request_schema;
      Alcotest.test_case "contains response schema" `Quick
        output_contains_response_schema;
      Alcotest.test_case "contains path and query parameters" `Quick
        output_contains_parameters;
      Alcotest.test_case "output is deterministic" `Quick
        output_is_deterministic;
      Alcotest.test_case "escapes strings" `Quick output_escapes_strings;
      Alcotest.test_case "empty API has empty paths" `Quick
        empty_api_has_empty_paths;
    ] )
