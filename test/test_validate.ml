open Contract

type create_user = { email : string; name : string option }

let ( let* ) = Result.bind

let create_user_codec =
  let schema =
    Schema.obj
      [ ("email", Schema.string, true); ("name", Schema.string, false) ]
  in
  let encode create_user =
    let fields = [ ("email", `String create_user.email) ] in
    match create_user.name with
    | None -> `Assoc fields
    | Some name -> `Assoc (fields @ [ ("name", `String name) ])
  in
  let decode json =
    let* email = Json_codec.required_field "email" Json_codec.string json in
    let* name = Json_codec.optional_field "name" Json_codec.string json in
    Ok { email; name }
  in
  Json_codec.make ~name:"CreateUser" ~schema ~encode ~decode ()

let expect_endpoint = function
  | Ok endpoint -> endpoint
  | Error error -> Alcotest.fail (Error.to_string error)

let get_user =
  Endpoint.get "/users/:id"
  |> Result.map (Endpoint.path_param "id" Codec.int)
  |> Result.map (Endpoint.query_param "include_deleted" Codec.bool)
  |> Result.map (Endpoint.response ~status:200 Json_codec.string)
  |> expect_endpoint

let get_user_required_query =
  Endpoint.get "/users/:id"
  |> Result.map (Endpoint.path_param "id" Codec.int)
  |> Result.map
       (Endpoint.query_param ~required:true "include_deleted" Codec.bool)
  |> Result.map (Endpoint.response ~status:200 Json_codec.string)
  |> expect_endpoint

let get_users_with_missing_path_value =
  Endpoint.get "/users"
  |> Result.map (Endpoint.path_param "id" Codec.int)
  |> Result.map (Endpoint.response ~status:200 Json_codec.string)
  |> expect_endpoint

let post_user =
  Endpoint.post "/users"
  |> Result.map (Endpoint.body create_user_codec)
  |> Result.map (Endpoint.response ~status:201 Json_codec.string)
  |> expect_endpoint

let expect_valid = function
  | Ok validated -> validated
  | Error errors ->
      errors |> List.map Error.to_string |> String.concat "\n" |> Alcotest.fail

let expect_error = function
  | Ok _ -> Alcotest.fail "expected validation to fail"
  | Error [] -> Alcotest.fail "expected at least one validation error"
  | Error _ -> ()

let expect_error_location expected = function
  | Ok _ -> Alcotest.fail "expected validation to fail"
  | Error [] -> Alcotest.fail "expected at least one validation error"
  | Error (error :: _) ->
      Alcotest.(check bool) "location" true (error.Error.location = expected)

let valid_get () =
  let request =
    Request.make ~method_:Endpoint.GET ~path:"/users/42"
      ~query:[ ("include_deleted", "false") ]
      ()
  in
  let validated = Validate.request get_user request |> expect_valid in
  begin match Validate.path validated "id" Codec.int with
  | Ok id -> Alcotest.(check int) "id" 42 id
  | Error error -> Alcotest.fail (Error.to_string error)
  end;
  match Validate.query validated "include_deleted" Codec.bool with
  | Ok include_deleted ->
      Alcotest.(check (option bool))
        "include_deleted" (Some false) include_deleted
  | Error error -> Alcotest.fail (Error.to_string error)

let undeclared_query_is_ignored () =
  Request.make ~method_:Endpoint.GET ~path:"/users/42"
    ~query:[ ("unused", "value") ]
    ()
  |> Validate.request get_user |> expect_valid |> ignore

let duplicate_query_uses_first_value () =
  let request =
    Request.make ~method_:Endpoint.GET ~path:"/users/42"
      ~query:[ ("include_deleted", "true"); ("include_deleted", "false") ]
      ()
  in
  let validated = Validate.request get_user request |> expect_valid in
  match Validate.query validated "include_deleted" Codec.bool with
  | Ok include_deleted ->
      Alcotest.(check (option bool))
        "include_deleted" (Some true) include_deleted
  | Error error -> Alcotest.fail (Error.to_string error)

let invalid_method () =
  Request.make ~method_:Endpoint.POST ~path:"/users/42" ()
  |> Validate.request get_user |> expect_error

let invalid_path () =
  Request.make ~method_:Endpoint.GET ~path:"/accounts/42" ()
  |> Validate.request get_user |> expect_error

let bad_path_param_type () =
  Request.make ~method_:Endpoint.GET ~path:"/users/not-an-int" ()
  |> Validate.request get_user |> expect_error

let bad_query_param_type () =
  Request.make ~method_:Endpoint.GET ~path:"/users/42"
    ~query:[ ("include_deleted", "no") ]
    ()
  |> Validate.request get_user
  |> expect_error_location (Error.Query_param "include_deleted")

let required_query_param_missing () =
  Request.make ~method_:Endpoint.GET ~path:"/users/42" ()
  |> Validate.request get_user_required_query
  |> expect_error_location (Error.Query_param "include_deleted")

let declared_path_param_must_match_template () =
  Request.make ~method_:Endpoint.GET ~path:"/users" ()
  |> Validate.request get_users_with_missing_path_value
  |> expect_error_location (Error.Path_param "id")

let valid_post_body () =
  let body = `Assoc [ ("email", `String "a@example.test") ] in
  Request.make ~method_:Endpoint.POST ~path:"/users" ~body ()
  |> Validate.request post_user |> expect_valid |> ignore

let invalid_json_body_field () =
  let body = `Assoc [ ("email", `Int 1) ] in
  Request.make ~method_:Endpoint.POST ~path:"/users" ~body ()
  |> Validate.request post_user
  |> expect_error_location (Error.Json_field "email")

let extra_json_body_fields_are_ignored () =
  let body =
    `Assoc [ ("email", `String "a@example.test"); ("role", `String "admin") ]
  in
  let validated =
    Request.make ~method_:Endpoint.POST ~path:"/users" ~body ()
    |> Validate.request post_user |> expect_valid
  in
  match Validate.body validated create_user_codec with
  | Ok (Some create_user) ->
      Alcotest.(check string) "email" "a@example.test" create_user.email;
      Alcotest.(check (option string)) "name" None create_user.name
  | Ok None -> Alcotest.fail "expected decoded request body"
  | Error error -> Alcotest.fail (Error.to_string error)

let missing_post_body () =
  Request.make ~method_:Endpoint.POST ~path:"/users" ()
  |> Validate.request post_user |> expect_error

let tests =
  ( "request validation",
    [
      Alcotest.test_case "valid GET" `Quick valid_get;
      Alcotest.test_case "ignores undeclared query params" `Quick
        undeclared_query_is_ignored;
      Alcotest.test_case "duplicate query uses first value" `Quick
        duplicate_query_uses_first_value;
      Alcotest.test_case "invalid method" `Quick invalid_method;
      Alcotest.test_case "invalid path" `Quick invalid_path;
      Alcotest.test_case "bad path param type" `Quick bad_path_param_type;
      Alcotest.test_case "bad query param type" `Quick bad_query_param_type;
      Alcotest.test_case "required query param missing" `Quick
        required_query_param_missing;
      Alcotest.test_case "declared path param must match template" `Quick
        declared_path_param_must_match_template;
      Alcotest.test_case "valid POST body" `Quick valid_post_body;
      Alcotest.test_case "invalid JSON body field" `Quick
        invalid_json_body_field;
      Alcotest.test_case "extra JSON body fields are ignored" `Quick
        extra_json_body_fields_are_ignored;
      Alcotest.test_case "missing POST body" `Quick missing_post_body;
    ] )
