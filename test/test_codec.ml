open Contract

let expect_ok expected = function
  | Ok got -> Alcotest.(check int) "decoded value" expected got
  | Error error -> Alcotest.fail (Error.to_string error)

let expect_bool expected = function
  | Ok got -> Alcotest.(check bool) "decoded value" expected got
  | Error error -> Alcotest.fail (Error.to_string error)

let expect_error = function
  | Ok _ -> Alcotest.fail "expected decode to fail"
  | Error _ -> ()

let int_success () = Codec.int.decode "42" |> expect_ok 42
let int_failure () = Codec.int.decode "abc" |> expect_error
let bool_success () = Codec.bool.decode "false" |> expect_bool false
let bool_failure () = Codec.bool.decode "yes" |> expect_error

let tests =
  ( "scalar codecs",
    [
      Alcotest.test_case "int success" `Quick int_success;
      Alcotest.test_case "int failure" `Quick int_failure;
      Alcotest.test_case "bool success" `Quick bool_success;
      Alcotest.test_case "bool failure" `Quick bool_failure;
    ] )
