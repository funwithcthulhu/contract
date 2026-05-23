open Contract

type create_user = {
  email : string;
  name : string option;
}

let ( let* ) = Result.bind

let create_user_codec =
  let schema =
    Schema.obj
      [
        ("email", Schema.string, true);
        ("name", Schema.string, false);
      ]
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

let get_user =
  Endpoint.get "/users/:id"
  |> Endpoint.path_param "id" Codec.int
  |> Endpoint.query_param "include_deleted" Codec.bool
  |> Endpoint.response ~status:200 Json_codec.string

let post_user =
  Endpoint.post "/users"
  |> Endpoint.body create_user_codec
  |> Endpoint.response ~status:201 Json_codec.string

let expect_valid = function
  | Ok validated -> validated
  | Error errors ->
      errors |> List.map Error.to_string |> String.concat "\n" |> Alcotest.fail

let expect_error = function
  | Ok _ -> Alcotest.fail "expected validation to fail"
  | Error [] -> Alcotest.fail "expected at least one validation error"
  | Error _ -> ()

let valid_get () =
  let request =
    Request.make ~meth:Endpoint.GET ~path:"/users/42"
      ~query:[ ("include_deleted", "false") ]
      ()
  in
  let validated = Validate.request get_user request |> expect_valid in
  begin
    match Validate.path validated "id" Codec.int with
    | Ok id -> Alcotest.(check int) "id" 42 id
    | Error error -> Alcotest.fail (Error.to_string error)
  end;
  match Validate.query validated "include_deleted" Codec.bool with
  | Ok include_deleted ->
      Alcotest.(check (option bool)) "include_deleted" (Some false)
        include_deleted
  | Error error -> Alcotest.fail (Error.to_string error)

let invalid_method () =
  Request.make ~meth:Endpoint.POST ~path:"/users/42" ()
  |> Validate.request get_user
  |> expect_error

let invalid_path () =
  Request.make ~meth:Endpoint.GET ~path:"/accounts/42" ()
  |> Validate.request get_user
  |> expect_error

let bad_path_param_type () =
  Request.make ~meth:Endpoint.GET ~path:"/users/not-an-int" ()
  |> Validate.request get_user
  |> expect_error

let bad_query_param_type () =
  Request.make ~meth:Endpoint.GET ~path:"/users/42"
    ~query:[ ("include_deleted", "no") ]
    ()
  |> Validate.request get_user
  |> expect_error

let valid_post_body () =
  let body = `Assoc [ ("email", `String "a@example.test") ] in
  Request.make ~meth:Endpoint.POST ~path:"/users" ~body ()
  |> Validate.request post_user
  |> expect_valid
  |> ignore

let missing_post_body () =
  Request.make ~meth:Endpoint.POST ~path:"/users" ()
  |> Validate.request post_user
  |> expect_error

let tests =
  ( "request validation",
    [
      Alcotest.test_case "valid GET" `Quick valid_get;
      Alcotest.test_case "invalid method" `Quick invalid_method;
      Alcotest.test_case "invalid path" `Quick invalid_path;
      Alcotest.test_case "bad path param type" `Quick bad_path_param_type;
      Alcotest.test_case "bad query param type" `Quick bad_query_param_type;
      Alcotest.test_case "valid POST body" `Quick valid_post_body;
      Alcotest.test_case "missing POST body" `Quick missing_post_body;
    ] )
