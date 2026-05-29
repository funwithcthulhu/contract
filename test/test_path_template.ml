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

let rejects_path_param ~message ?expected ?got template path =
  match Path_template.match_path template path with
  | Ok _ -> Alcotest.fail "expected path match to fail"
  | Error error ->
      Alcotest.(check bool)
        "location" true
        (error.Error.location = Error.Path_param "id");
      Alcotest.(check string) "message" message error.message;
      Alcotest.(check (option string)) "expected" expected error.expected;
      Alcotest.(check (option string)) "got" got error.got

let parses_user_id () =
  let template = parse_exn "/users/:id" in
  Alcotest.(check string) "raw" "/users/:id" (Path_template.raw template)

let matches_user_id () =
  let template = parse_exn "/users/:id" in
  Alcotest.(check (list (pair string string)))
    "params"
    [ ("id", "42") ]
    (matches_exn template "/users/42")

let decodes_percent_encoded_space () =
  let template = parse_exn "/users/:id" in
  Alcotest.(check (list (pair string string)))
    "params"
    [ ("id", "alice smith") ]
    (matches_exn template "/users/alice%20smith")

let encoded_slash_stays_inside_path_param () =
  let template = parse_exn "/files/:path" in
  Alcotest.(check (list (pair string string)))
    "params"
    [ ("path", "a/b") ]
    (matches_exn template "/files/a%2Fb")

let decodes_lowercase_and_uppercase_hex () =
  let template = parse_exn "/users/:id" in
  Alcotest.(check (list (pair string string)))
    "params"
    [ ("id", "~~") ]
    (matches_exn template "/users/%7e%7E")

let literal_slash_still_splits_path_segments () =
  parse_exn "/files/:path" |> fun template -> rejects template "/files/a/b"

let literal_segment_is_matched_without_decoding () =
  let template = parse_exn "/users/alice%20smith" in
  Alcotest.(check (list (pair string string)))
    "params" []
    (matches_exn template "/users/alice%20smith");
  rejects template "/users/alice smith"

let rejects_percent_at_end () =
  let template = parse_exn "/users/:id" in
  rejects_path_param ~message:"malformed percent escape" ~expected:"%HH"
    ~got:"alice%" template "/users/alice%"

let rejects_non_hex_percent_escape () =
  let template = parse_exn "/users/:id" in
  rejects_path_param ~message:"malformed percent escape" ~expected:"%HH"
    ~got:"alice%G0" template "/users/alice%G0"

let rejects_invalid_utf8_percent_sequence () =
  let template = parse_exn "/users/:id" in
  rejects_path_param ~message:"invalid UTF-8 path parameter"
    ~expected:"valid UTF-8" ~got:"%c3%28" template "/users/%c3%28"

let rejects_short_path () =
  parse_exn "/users/:id" |> fun template -> rejects template "/users"

let rejects_long_path () =
  parse_exn "/users/:id" |> fun template -> rejects template "/users/42/posts"

let converts_to_openapi_path () =
  let template = parse_exn "/users/:id" in
  Alcotest.(check string)
    "openapi path" "/users/{id}"
    (Path_template.to_openapi_path template)

let tests =
  ( "path templates",
    [
      Alcotest.test_case "parses /users/:id" `Quick parses_user_id;
      Alcotest.test_case "matches /users/42" `Quick matches_user_id;
      Alcotest.test_case "decodes %20 in path param" `Quick
        decodes_percent_encoded_space;
      Alcotest.test_case "decodes %2F inside path param" `Quick
        encoded_slash_stays_inside_path_param;
      Alcotest.test_case "decodes lowercase and uppercase hex" `Quick
        decodes_lowercase_and_uppercase_hex;
      Alcotest.test_case "literal slash still splits path segments" `Quick
        literal_slash_still_splits_path_segments;
      Alcotest.test_case "literal segment is not percent-decoded" `Quick
        literal_segment_is_matched_without_decoding;
      Alcotest.test_case "rejects percent at end" `Quick rejects_percent_at_end;
      Alcotest.test_case "rejects non-hex percent escape" `Quick
        rejects_non_hex_percent_escape;
      Alcotest.test_case "rejects invalid UTF-8 percent sequence" `Quick
        rejects_invalid_utf8_percent_sequence;
      Alcotest.test_case "rejects missing segment" `Quick rejects_short_path;
      Alcotest.test_case "rejects extra segment" `Quick rejects_long_path;
      Alcotest.test_case "converts to OpenAPI path" `Quick
        converts_to_openapi_path;
    ] )
