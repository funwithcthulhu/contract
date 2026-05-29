open Contract

type error_payload = { code : string; detail : string }

let ( let* ) = Result.bind

let error_payload_codec =
  let schema =
    Schema.obj
      [ ("code", Schema.string, true); ("detail", Schema.string, true) ]
  in
  let encode error =
    `Assoc [ ("code", `String error.code); ("detail", `String error.detail) ]
  in
  let decode json =
    let* code = Json_codec.required_field "code" Json_codec.string json in
    let* detail = Json_codec.required_field "detail" Json_codec.string json in
    Ok { code; detail }
  in
  Json_codec.make ~name:"ErrorPayload" ~schema ~encode ~decode ()

let expect_endpoint = function
  | Ok endpoint -> endpoint
  | Error error -> Alcotest.fail (Error.to_string error)

let user_response =
  Endpoint.get "/users/:id"
  |> Result.map (Endpoint.path_param "id" Codec.int)
  |> Result.map (Endpoint.response ~status:200 Json_codec.string)
  |> Result.map (Endpoint.empty_response ~status:404)
  |> expect_endpoint

let create_response =
  Endpoint.post "/users"
  |> Result.map (Endpoint.response ~status:201 Json_codec.int)
  |> expect_endpoint

let create_payment =
  Endpoint.post "/payments"
  |> Result.map (Endpoint.response ~status:201 Json_codec.string)
  |> Result.map (Endpoint.response ~status:422 error_payload_codec)
  |> expect_endpoint

let status_responses =
  Endpoint.get "/status"
  |> Result.map (Endpoint.response ~status:200 Json_codec.string)
  |> Result.map (Endpoint.response ~status:201 Json_codec.int)
  |> Result.map (Endpoint.empty_response ~status:204)
  |> Result.map (Endpoint.response ~status:400 Json_codec.string)
  |> expect_endpoint

let response_codec_endpoint =
  Endpoint.post "/decode"
  |> Result.map (Endpoint.body Json_codec.string)
  |> Result.map (Endpoint.response ~status:201 Json_codec.int)
  |> expect_endpoint

let expect_valid = function
  | Ok response -> response
  | Error errors ->
      errors |> List.map Error.to_string |> String.concat "\n" |> Alcotest.fail

let expect_error expected = function
  | Ok _ -> Alcotest.fail "expected response validation to fail"
  | Error [] -> Alcotest.fail "expected at least one validation error"
  | Error (error :: _) ->
      Alcotest.(check bool)
        "location" true
        (error.Error.location = expected.Error.location);
      Alcotest.(check string) "message" expected.message error.message;
      Alcotest.(check (option string))
        "expected" expected.expected error.expected;
      Alcotest.(check (option string)) "got" expected.got error.got

let response_encode_succeeds () =
  let body = Json_codec.string.encode "alice" in
  let response = Response.make ~status:200 ~body () in
  let validated = Validate.response user_response response |> expect_valid in
  match Validate.response_body validated Json_codec.string with
  | Ok body -> Alcotest.(check (option string)) "body" (Some "alice") body
  | Error error -> Alcotest.fail (Error.to_string error)

let valid_json_body () =
  let response = Response.make ~status:200 ~body:(`String "alice") () in
  let validated = Validate.response user_response response |> expect_valid in
  Alcotest.(check int) "status" 200 validated.status;
  match Validate.response_body validated Json_codec.string with
  | Ok body -> Alcotest.(check (option string)) "body" (Some "alice") body
  | Error error -> Alcotest.fail (Error.to_string error)

let valid_empty_body () =
  let validated =
    Response.make ~status:404 ()
    |> Validate.response user_response
    |> expect_valid
  in
  match Validate.response_body validated Json_codec.string with
  | Ok body -> Alcotest.(check (option string)) "body" None body
  | Error error -> Alcotest.fail (Error.to_string error)

let declared_non_2xx_status_is_accepted () =
  let response = Response.make ~status:400 ~body:(`String "bad request") () in
  let validated = Validate.response status_responses response |> expect_valid in
  Alcotest.(check int) "status" 400 validated.status;
  match Validate.response_body validated Json_codec.string with
  | Ok body -> Alcotest.(check (option string)) "body" (Some "bad request") body
  | Error error -> Alcotest.fail (Error.to_string error)

let declared_422_response_decodes_error_payload () =
  let body =
    `Assoc
      [
        ("code", `String "card_declined");
        ("detail", `String "The issuer declined the charge");
      ]
  in
  let validated =
    Response.make ~status:422 ~body ()
    |> Validate.response create_payment
    |> expect_valid
  in
  match Validate.response_body validated error_payload_codec with
  | Ok (Some error) ->
      Alcotest.(check string) "code" "card_declined" error.code;
      Alcotest.(check string)
        "detail" "The issuer declined the charge" error.detail
  | Ok None -> Alcotest.fail "expected decoded error payload"
  | Error error -> Alcotest.fail (Error.to_string error)

let valid_no_content_status () =
  Response.make ~status:204 ()
  |> Validate.response status_responses
  |> expect_valid |> ignore

let unexpected_status () =
  Response.make ~status:500 ()
  |> Validate.response user_response
  |> expect_error
       (Error.make ~location:Error.Status ~expected:"200, 404" ~got:"500"
          "unexpected response status")

let undeclared_2xx_status_is_rejected () =
  Response.make ~status:202 ()
  |> Validate.response status_responses
  |> expect_error
       (Error.make ~location:Error.Status ~expected:"200, 201, 204, 400"
          ~got:"202" "unexpected response status")

let missing_body () =
  Response.make ~status:200 ()
  |> Validate.response user_response
  |> expect_error (Error.make ~location:Error.Body "missing response body")

let bad_json_body () =
  Response.make ~status:201 ~body:(`String "not an int") ()
  |> Validate.response create_response
  |> expect_error
       (Error.make ~location:Error.Body ~expected:"integer" ~got:"string"
          "expected integer")

let unexpected_body () =
  Response.make ~status:404 ~body:(`String "not found") ()
  |> Validate.response user_response
  |> expect_error (Error.make ~location:Error.Body "unexpected response body")

let exact_status_selects_response_codec () =
  let response = Response.make ~status:201 ~body:(`Int 42) () in
  let validated = Validate.response status_responses response |> expect_valid in
  match Validate.response_body validated Json_codec.int with
  | Ok body -> Alcotest.(check (option int)) "body" (Some 42) body
  | Error error -> Alcotest.fail (Error.to_string error)

let request_body_codec_is_not_used () =
  Response.make ~status:201 ~body:(`Int 42) ()
  |> Validate.response response_codec_endpoint
  |> expect_valid |> ignore

let tests =
  ( "response validation",
    [
      Alcotest.test_case "response encode succeeds" `Quick
        response_encode_succeeds;
      Alcotest.test_case "valid JSON body" `Quick valid_json_body;
      Alcotest.test_case "valid empty body" `Quick valid_empty_body;
      Alcotest.test_case "declared non-2xx status is accepted" `Quick
        declared_non_2xx_status_is_accepted;
      Alcotest.test_case "declared 422 response decodes error payload" `Quick
        declared_422_response_decodes_error_payload;
      Alcotest.test_case "valid no-content status" `Quick
        valid_no_content_status;
      Alcotest.test_case "unexpected status" `Quick unexpected_status;
      Alcotest.test_case "undeclared 2xx status is rejected" `Quick
        undeclared_2xx_status_is_rejected;
      Alcotest.test_case "missing body" `Quick missing_body;
      Alcotest.test_case "bad JSON body" `Quick bad_json_body;
      Alcotest.test_case "unexpected body" `Quick unexpected_body;
      Alcotest.test_case "exact status selects response codec" `Quick
        exact_status_selects_response_codec;
      Alcotest.test_case "request body codec is not used" `Quick
        request_body_codec_is_not_used;
    ] )
