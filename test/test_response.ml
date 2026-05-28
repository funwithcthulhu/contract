open Contract

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

let expect_valid = function
  | Ok response -> response
  | Error errors ->
      errors |> List.map Error.to_string |> String.concat "\n" |> Alcotest.fail

let expect_error_location expected = function
  | Ok _ -> Alcotest.fail "expected response validation to fail"
  | Error [] -> Alcotest.fail "expected at least one validation error"
  | Error (error :: _) ->
      Alcotest.(check bool)
        "location" true (error.Error.location = expected)

let valid_json_body () =
  let response = Response.make ~status:200 ~body:(`String "alice") () in
  let validated = Validate.response user_response response |> expect_valid in
  Alcotest.(check int) "status" 200 validated.status;
  match Validate.response_body validated Json_codec.string with
  | Ok body -> Alcotest.(check (option string)) "body" (Some "alice") body
  | Error error -> Alcotest.fail (Error.to_string error)

let valid_empty_body () =
  let validated =
    Response.make ~status:404 () |> Validate.response user_response |> expect_valid
  in
  match Validate.response_body validated Json_codec.string with
  | Ok body -> Alcotest.(check (option string)) "body" None body
  | Error error -> Alcotest.fail (Error.to_string error)

let unexpected_status () =
  Response.make ~status:500 ()
  |> Validate.response user_response
  |> expect_error_location Error.Status

let missing_body () =
  Response.make ~status:200 ()
  |> Validate.response user_response
  |> expect_error_location Error.Body

let bad_json_body () =
  Response.make ~status:201 ~body:(`String "not an int") ()
  |> Validate.response create_response
  |> expect_error_location Error.Body

let unexpected_body () =
  Response.make ~status:404 ~body:(`String "not found") ()
  |> Validate.response user_response
  |> expect_error_location Error.Body

let tests =
  ( "response validation",
    [
      Alcotest.test_case "valid JSON body" `Quick valid_json_body;
      Alcotest.test_case "valid empty body" `Quick valid_empty_body;
      Alcotest.test_case "unexpected status" `Quick unexpected_status;
      Alcotest.test_case "missing body" `Quick missing_body;
      Alcotest.test_case "bad JSON body" `Quick bad_json_body;
      Alcotest.test_case "unexpected body" `Quick unexpected_body;
    ] )
