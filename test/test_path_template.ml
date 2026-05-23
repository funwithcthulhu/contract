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
      Alcotest.test_case "rejects missing segment" `Quick rejects_short_path;
      Alcotest.test_case "rejects extra segment" `Quick rejects_long_path;
      Alcotest.test_case "converts to OpenAPI path" `Quick
        converts_to_openapi_path;
    ] )
