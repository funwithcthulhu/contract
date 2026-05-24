open Contract

let parse_exn path =
  match Path_template.parse path with
  | Ok template -> template
  | Error error -> Alcotest.fail (Error.to_string error)

let matches_exn template path =
  match Path_template.match_path template path with
  | Ok values -> values
  | Error error -> Alcotest.fail (Error.to_string error)

let rejects template path =
  match Path_template.match_path template path with
  | Ok _ -> Alcotest.fail "expected path match to fail"
  | Error _ -> ()

let parses_user_id () =
  let template = parse_exn "/users/:id" in
  Alcotest.(check string) "raw" "/users/:id" (Path_template.raw template)

let matches_user_id () =
  let template = parse_exn "/users/:id" in
  Alcotest.(check (list (pair string string)))
    "params" [ ("id", "42") ] (matches_exn template "/users/42")

let decodes_percent_encoded_space () =
  let template = parse_exn "/users/:id" in
  Alcotest.(check (list (pair string string)))
    "params" [ ("id", "alice smith") ]
    (matches_exn template "/users/alice%20smith")

let encoded_slash_stays_inside_path_param () =
  let template = parse_exn "/files/:path" in
  Alcotest.(check (list (pair string string)))
    "params" [ ("path", "a/b") ] (matches_exn template "/files/a%2Fb")

let literal_slash_still_splits_path_segments () =
  parse_exn "/files/:path" |> fun template -> rejects template "/files/a/b"

let rejects_malformed_percent_escape () =
  let template = parse_exn "/users/:id" in
  match Path_template.match_path template "/users/alice%2" with
  | Ok _ -> Alcotest.fail "expected malformed percent escape to fail"
  | Error error ->
      Alcotest.(check bool)
        "location" true (error.Error.location = Error.Path_param "id");
      Alcotest.(check string) "message" "malformed percent escape"
        error.message;
      Alcotest.(check (option string)) "expected" (Some "%HH") error.expected;
      Alcotest.(check (option string)) "got" (Some "alice%2") error.got

let rejects_short_path () =
  parse_exn "/users/:id" |> fun template -> rejects template "/users"

let rejects_long_path () =
  parse_exn "/users/:id" |> fun template -> rejects template "/users/42/posts"

let converts_to_openapi_path () =
  let template = parse_exn "/users/:id" in
  Alcotest.(check string)
    "openapi path" "/users/{id}" (Path_template.to_openapi_path template)

let tests =
  ( "path templates",
    [
      Alcotest.test_case "parses /users/:id" `Quick parses_user_id;
      Alcotest.test_case "matches /users/42" `Quick matches_user_id;
      Alcotest.test_case "decodes %20 in path param" `Quick
        decodes_percent_encoded_space;
      Alcotest.test_case "decodes %2F inside path param" `Quick
        encoded_slash_stays_inside_path_param;
      Alcotest.test_case "literal slash still splits path segments" `Quick
        literal_slash_still_splits_path_segments;
      Alcotest.test_case "rejects malformed percent escape" `Quick
        rejects_malformed_percent_escape;
      Alcotest.test_case "rejects missing segment" `Quick rejects_short_path;
      Alcotest.test_case "rejects extra segment" `Quick rejects_long_path;
      Alcotest.test_case "converts to OpenAPI path" `Quick
        converts_to_openapi_path;
    ] )
