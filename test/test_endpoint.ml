open Contract

let expect_endpoint = function
  | Ok endpoint -> endpoint
  | Error error -> Alcotest.fail (Error.to_string error)

let expect_error = function
  | Ok _ -> Alcotest.fail "expected endpoint construction to fail"
  | Error _ -> ()

let get_validates_template () =
  let endpoint = Endpoint.get "/users/:id" |> expect_endpoint in
  Alcotest.(check string)
    "path" "/users/:id" (Path_template.raw endpoint.Endpoint.path)

let get_rejects_invalid_template () =
  Endpoint.get "/users//:id" |> expect_error

let method_name () =
  Alcotest.(check string)
    "method" "GET" (Endpoint.method_to_string Endpoint.GET)

let tests =
  ( "endpoints",
    [
      Alcotest.test_case "constructs from valid template" `Quick
        get_validates_template;
      Alcotest.test_case "rejects invalid template" `Quick
        get_rejects_invalid_template;
      Alcotest.test_case "method name" `Quick method_name;
    ] )
