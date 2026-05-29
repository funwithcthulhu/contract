open Contract

let expect_string expected = function
  | Ok got -> Alcotest.(check string) "decoded value" expected got
  | Error error -> Alcotest.fail (Error.to_string error)

let expect_string_option expected = function
  | Ok got -> Alcotest.(check (option string)) "decoded value" expected got
  | Error error -> Alcotest.fail (Error.to_string error)

let expect_error = function
  | Ok _ -> Alcotest.fail "expected decode to fail"
  | Error _ -> ()

let string_success () =
  Json_codec.string.decode (`String "alice") |> expect_string "alice"

let string_failure () = Json_codec.string.decode (`Int 1) |> expect_error

let required_field_success () =
  let json = `Assoc [ ("email", `String "a@example.test") ] in
  Json_codec.required_field "email" Json_codec.string json
  |> expect_string "a@example.test"

let required_field_missing () =
  Json_codec.required_field "email" Json_codec.string (`Assoc [])
  |> expect_error

let optional_field_missing () =
  Json_codec.optional_field "name" Json_codec.string (`Assoc [])
  |> expect_string_option None

let optional_field_null () =
  let json = `Assoc [ ("name", `Null) ] in
  Json_codec.optional_field "name" Json_codec.string json
  |> expect_string_option None

let tests =
  ( "json codecs",
    [
      Alcotest.test_case "string success" `Quick string_success;
      Alcotest.test_case "string failure" `Quick string_failure;
      Alcotest.test_case "required field success" `Quick required_field_success;
      Alcotest.test_case "required field missing" `Quick required_field_missing;
      Alcotest.test_case "optional field missing" `Quick optional_field_missing;
      Alcotest.test_case "optional field null" `Quick optional_field_null;
    ] )
