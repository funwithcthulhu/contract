open Contract

let expect_endpoint = function
  | Ok endpoint -> endpoint
  | Error error -> Alcotest.fail (Error.to_string error)

let expect_api = function
  | Ok api -> api
  | Error error -> Alcotest.fail (Error.to_string error)

let expect_error_string expected = function
  | Ok _ -> Alcotest.fail "expected API construction or matching to fail"
  | Error error ->
      Alcotest.(check string) "error" expected (Error.to_string error)

let text_response endpoint =
  Endpoint.response ~status:200 Json_codec.string endpoint

let get_user =
  Endpoint.get "/users/:id"
  |> Result.map (Endpoint.path_param "id" Codec.string)
  |> Result.map text_response |> expect_endpoint

let get_current_user =
  Endpoint.get "/users/me" |> Result.map text_response |> expect_endpoint

let get_users =
  Endpoint.get "/users" |> Result.map text_response |> expect_endpoint

let post_users =
  Endpoint.post "/users" |> Result.map text_response |> expect_endpoint

let get_user_by_name =
  Endpoint.get "/users/:name"
  |> Result.map (Endpoint.path_param "name" Codec.string)
  |> Result.map text_response |> expect_endpoint

let api endpoints =
  Api.make ~title:"Users API" ~version:"0.3.0" endpoints |> expect_api

let make_accepts_same_path_with_different_methods () =
  let api = api [ get_users; post_users ] in
  Alcotest.(check int) "endpoint count" 2 (List.length (Api.endpoints api))

let make_rejects_duplicate_method_path () =
  Api.make ~title:"Users API" ~version:"0.3.0" [ get_user; get_user ]
  |> expect_error_string
       "route: duplicate endpoint declaration (got: GET /users/:id)"

let make_rejects_ambiguous_param_route_names () =
  Api.make ~title:"Users API" ~version:"0.3.0" [ get_user; get_user_by_name ]
  |> expect_error_string
       "route: ambiguous endpoint route (got: GET /users/:id conflicts with \
        GET /users/:name)"

let static_route_precedes_param_route () =
  let api = api [ get_user; get_current_user ] in
  let request = Request.make ~method_:Endpoint.GET ~path:"/users/me" () in
  match Api.match_request api request with
  | Ok endpoint ->
      Alcotest.(check string)
        "route" "/users/me"
        (Path_template.raw endpoint.path)
  | Error error -> Alcotest.fail (Error.to_string error)

let param_route_matches_when_static_route_does_not () =
  let api = api [ get_current_user; get_user ] in
  let request = Request.make ~method_:Endpoint.GET ~path:"/users/alice" () in
  match Api.match_request api request with
  | Ok endpoint ->
      Alcotest.(check string)
        "route" "/users/:id"
        (Path_template.raw endpoint.path)
  | Error error -> Alcotest.fail (Error.to_string error)

let method_mismatch_reports_allowed_methods () =
  let api = api [ get_users; post_users ] in
  Request.make ~method_:Endpoint.DELETE ~path:"/users" ()
  |> Api.match_request api
  |> expect_error_string
       "method: HTTP method does not match any endpoint for path (expected: \
        GET, POST, got: DELETE)"

let no_route_match_reports_path () =
  let api = api [ get_users ] in
  Request.make ~method_:Endpoint.GET ~path:"/accounts" ()
  |> Api.match_request api
  |> expect_error_string
       "route: path does not match any endpoint (got: /accounts)"

let malformed_percent_escape_is_reported () =
  let api = api [ get_user ] in
  Request.make ~method_:Endpoint.GET ~path:"/users/a%zz" ()
  |> Api.match_request api
  |> expect_error_string
       "path parameter id: malformed percent escape (expected: %HH, got: a%zz)"

let tests =
  ( "api",
    [
      Alcotest.test_case "accepts same path with different methods" `Quick
        make_accepts_same_path_with_different_methods;
      Alcotest.test_case "rejects duplicate method and path" `Quick
        make_rejects_duplicate_method_path;
      Alcotest.test_case "rejects ambiguous param route names" `Quick
        make_rejects_ambiguous_param_route_names;
      Alcotest.test_case "static route precedes param route" `Quick
        static_route_precedes_param_route;
      Alcotest.test_case "param route matches when static route does not" `Quick
        param_route_matches_when_static_route_does_not;
      Alcotest.test_case "method mismatch reports allowed methods" `Quick
        method_mismatch_reports_allowed_methods;
      Alcotest.test_case "no route match reports path" `Quick
        no_route_match_reports_path;
      Alcotest.test_case "malformed percent escape is reported" `Quick
        malformed_percent_escape_is_reported;
    ] )
