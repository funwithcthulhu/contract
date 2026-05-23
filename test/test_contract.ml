let () =
  Alcotest.run "contract"
    [
      Test_path_template.tests;
      Test_codec.tests;
      Test_json_codec.tests;
      Test_validate.tests;
      Test_openapi.tests;
    ]
