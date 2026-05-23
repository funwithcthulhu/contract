open Contract

let user_codec =
  let schema =
    Schema.obj
      [
        ("id", Schema.integer, true);
        ("email", Schema.string, true);
      ]
  in
  Json_codec.make ~name:"User" ~schema ~encode:(fun () -> `Assoc [])
    ~decode:(fun _ -> Ok ())
    ()

let create_user_codec =
  let schema = Schema.obj [ ("email", Schema.string, true) ] in
  Json_codec.make ~name:"CreateUser" ~schema ~encode:(fun () -> `Assoc [])
    ~decode:(fun _ -> Ok ())
    ()

let get_user =
  Endpoint.get ~summary:"Fetch a user" ~operation_id:"getUser" "/users/:id"
  |> Endpoint.path_param "id" Codec.int
  |> Endpoint.response ~status:200 user_codec

let post_user =
  Endpoint.post ~summary:"Create a user" ~operation_id:"createUser" "/users"
  |> Endpoint.body create_user_codec
  |> Endpoint.response ~status:201 user_codec

let api : Openapi.api =
  { title = "Users API"; version = "0.1.0"; endpoints = [ get_user; post_user ] }

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

let output_contains_openapi_version () =
  let json = Openapi.to_yojson api in
  match require_member "openapi" json with
  | `String version -> Alcotest.(check string) "version" "3.0.3" version
  | _ -> Alcotest.fail "openapi should be a string"

let output_contains_users_id_path () =
  Openapi.to_yojson api |> require_path "/users/{id}" |> ignore

let output_contains_get_operation () =
  Openapi.to_yojson api
  |> require_path "/users/{id}"
  |> require_member "get"
  |> ignore

let output_contains_post_operation () =
  Openapi.to_yojson api |> require_path "/users" |> require_member "post" |> ignore

let output_contains_post_request_body () =
  Openapi.to_yojson api
  |> require_path "/users"
  |> require_member "post"
  |> require_member "requestBody"
  |> ignore

let output_contains_response_200 () =
  Openapi.to_yojson api
  |> require_path "/users/{id}"
  |> require_member "get"
  |> require_member "responses"
  |> require_member "200"
  |> ignore

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
      Alcotest.test_case "contains POST requestBody" `Quick
        output_contains_post_request_body;
      Alcotest.test_case "contains response 200" `Quick output_contains_response_200;
    ] )
