open Contract

type user = { id : int; email : string; name : string option }
type create_user = { email : string; name : string option }
type error_payload = { code : string; detail : string }

type request = {
  meth : string;
  path : string;
  query : (string * string list) list;
  headers : (string * string) list;
  body : Yojson.Safe.t option;
}

type response = {
  status : int;
  headers : (string * string) list;
  body : Yojson.Safe.t option;
}

type route = { endpoint : Endpoint.t; handle : Validate.validated -> response }

let ( let* ) = Result.bind

let or_exit = function
  | Ok value -> value
  | Error error ->
      prerr_endline (Error.to_string error);
      exit 1

let json_headers = [ ("content-type", "application/json") ]

let assoc_with_optional_name fields = function
  | None -> `Assoc fields
  | Some name -> `Assoc (fields @ [ ("name", `String name) ])

let user_schema =
  Schema.obj
    [
      ("id", Schema.integer, true);
      ("email", Schema.string, true);
      ("name", Schema.string, false);
    ]

let user_codec =
  let encode user =
    assoc_with_optional_name
      [ ("id", `Int user.id); ("email", `String user.email) ]
      user.name
  in
  let decode json =
    let* id = Json_codec.required_field "id" Json_codec.int json in
    let* email = Json_codec.required_field "email" Json_codec.string json in
    let* name = Json_codec.optional_field "name" Json_codec.string json in
    Ok { id; email; name }
  in
  Json_codec.make ~name:"User" ~schema:user_schema ~encode ~decode ()

let create_user_schema =
  Schema.obj [ ("email", Schema.string, true); ("name", Schema.string, false) ]

let create_user_codec =
  let encode create_user =
    assoc_with_optional_name
      [ ("email", `String create_user.email) ]
      create_user.name
  in
  let decode json =
    let* email = Json_codec.required_field "email" Json_codec.string json in
    let* name = Json_codec.optional_field "name" Json_codec.string json in
    Ok { email; name }
  in
  Json_codec.make ~name:"CreateUser" ~schema:create_user_schema ~encode ~decode
    ()

let error_schema =
  Schema.obj [ ("code", Schema.string, true); ("detail", Schema.string, true) ]

let error_codec =
  let encode error =
    `Assoc [ ("code", `String error.code); ("detail", `String error.detail) ]
  in
  let decode json =
    let* code = Json_codec.required_field "code" Json_codec.string json in
    let* detail = Json_codec.required_field "detail" Json_codec.string json in
    Ok { code; detail }
  in
  Json_codec.make ~name:"Error" ~schema:error_schema ~encode ~decode ()

let get_current_user =
  Endpoint.get ~summary:"Fetch the current user" ~operation_id:"getCurrentUser"
    "/users/me"
  |> Result.map (Endpoint.response ~status:200 user_codec)
  |> or_exit

let get_user =
  Endpoint.get ~summary:"Fetch a user" ~operation_id:"getUser" "/users/:id"
  |> Result.map (Endpoint.path_param "id" Codec.int)
  |> Result.map (Endpoint.query_param "include_deleted" Codec.bool)
  |> Result.map (Endpoint.response ~status:200 user_codec)
  |> Result.map (Endpoint.response ~status:404 error_codec)
  |> or_exit

let create_user =
  Endpoint.post ~summary:"Create a user" ~operation_id:"createUser" "/users"
  |> Result.map (Endpoint.body create_user_codec)
  |> Result.map (Endpoint.response ~status:201 user_codec)
  |> or_exit

let error_json code detail = error_codec.Json_codec.encode { code; detail }

let user_response ~status user =
  { status; headers = json_headers; body = Some (user_codec.encode user) }

let adapter_error status code errors =
  let details =
    errors |> List.map (fun error -> `String (Error.to_string error))
  in
  {
    status;
    headers = json_headers;
    body = Some (`Assoc [ ("error", `String code); ("details", `List details) ]);
  }

let request_error_response = function
  | [] -> adapter_error 500 "adapter_error" []
  | error :: _ as errors -> (
      match error.Error.location with
      | Error.Method -> adapter_error 405 "method_not_allowed" errors
      | Error.Route -> adapter_error 404 "not_found" errors
      | Error.Path_param _ | Query_param _ | Body | Json_field _ ->
          adapter_error 400 "bad_request" errors
      | Error.Status -> adapter_error 500 "response_validation_failed" errors)

let response_error_response errors =
  adapter_error 500 "response_validation_failed" errors

let internal_error message =
  let error = Error.make ~location:Error.Body message in
  adapter_error 500 "adapter_error" [ error ]

let current_user_handler _validated =
  user_response ~status:200
    { id = 1; email = "me@example.test"; name = Some "Current User" }

let get_user_handler validated =
  match
    ( Validate.path validated "id" Codec.int,
      Validate.query validated "include_deleted" Codec.bool )
  with
  | Ok 404, _ ->
      {
        status = 404;
        headers = json_headers;
        body = Some (error_json "not_found" "user not found");
      }
  | Ok 500, _ ->
      {
        status = 500;
        headers = json_headers;
        body = Some (error_json "storage_failed" "upstream lookup failed");
      }
  | Ok 2, _ ->
      {
        status = 200;
        headers = json_headers;
        body =
          Some
            (`Assoc
               [
                 ("id", `String "2");
                 ("email", `String "broken@example.test");
                 ("name", `String "Broken User");
               ]);
      }
  | Ok id, Ok include_deleted ->
      let name =
        match include_deleted with
        | Some true -> Some "Visible Deleted User"
        | _ -> Some "Example User"
      in
      user_response ~status:200
        { id; email = "user" ^ string_of_int id ^ "@example.test"; name }
  | Error error, _ | _, Error error ->
      internal_error ("handler decode failed: " ^ Error.to_string error)

let create_user_handler validated =
  match Validate.body validated create_user_codec with
  | Ok (Some input) ->
      user_response ~status:201
        { id = 100; email = input.email; name = input.name }
  | Ok None -> internal_error "handler expected a decoded request body"
  | Error error ->
      internal_error ("handler decode failed: " ^ Error.to_string error)

let routes =
  [
    { endpoint = get_current_user; handle = current_user_handler };
    { endpoint = get_user; handle = get_user_handler };
    { endpoint = create_user; handle = create_user_handler };
  ]

let api =
  routes
  |> List.map (fun route -> route.endpoint)
  |> Api.make ~title:"HTTP Style Users API" ~version:"0.3.0"
  |> or_exit

let method_of_string = function
  | "GET" -> Ok Endpoint.GET
  | "POST" -> Ok Endpoint.POST
  | "PUT" -> Ok Endpoint.PUT
  | "PATCH" -> Ok Endpoint.PATCH
  | "DELETE" -> Ok Endpoint.DELETE
  | meth ->
      Error
        (Error.make ~location:Error.Method ~got:meth "unsupported HTTP method")

let flatten_query query =
  List.concat_map
    (fun (name, values) -> List.map (fun value -> (name, value)) values)
    query

let contract_request (request : request) =
  let _headers = request.headers in
  match method_of_string request.meth with
  | Error error -> Error error
  | Ok method_ ->
      let query = flatten_query request.query in
      Ok
        (match request.body with
        | None -> Request.make ~method_ ~path:request.path ~query ()
        | Some body -> Request.make ~method_ ~path:request.path ~query ~body ())

let same_endpoint left right =
  left.Endpoint.method_ = right.Endpoint.method_
  && String.equal
       (Path_template.raw left.Endpoint.path)
       (Path_template.raw right.Endpoint.path)

let route_for endpoint =
  routes |> List.find_opt (fun route -> same_endpoint route.endpoint endpoint)

let contract_response (response : response) =
  match response.body with
  | None -> Response.make ~status:response.status ()
  | Some body -> Response.make ~status:response.status ~body ()

let dispatch request =
  match contract_request request with
  | Error error -> request_error_response [ error ]
  | Ok incoming -> (
      match Validate.api_request api incoming with
      | Error errors -> request_error_response errors
      | Ok validated -> (
          match route_for validated.endpoint with
          | None -> internal_error "validated endpoint has no handler"
          | Some route -> (
              let response = route.handle validated in
              match
                Validate.response validated.endpoint
                  (contract_response response)
              with
              | Ok _ -> response
              | Error errors -> response_error_response errors)))

let response_to_yojson (response : response) =
  let headers =
    response.headers
    |> List.map (fun (name, value) ->
        `Assoc [ ("name", `String name); ("value", `String value) ])
  in
  `Assoc
    [
      ("status", `Int response.status);
      ("headers", `List headers);
      ("body", Option.value response.body ~default:`Null);
    ]

let print_json label json =
  print_endline ("== " ^ label ^ " ==");
  print_endline (Yojson.Safe.pretty_to_string json)

let run_case label request =
  dispatch request |> response_to_yojson |> print_json label

let request ?(query = []) ?body meth path =
  { meth; path; query; headers = []; body }

let () =
  api |> Openapi.of_api |> Openapi.to_yojson |> print_json "openapi";
  run_case "static route" (request "GET" "/users/me");
  run_case "path and query params"
    (request "GET" "/users/42" ~query:[ ("include_deleted", [ "true" ]) ]);
  run_case "json request body"
    (request "POST" "/users"
       ~body:
         (`Assoc
            [ ("email", `String "new@example.test"); ("name", `String "New") ]));
  run_case "no route match" (request "GET" "/accounts");
  run_case "bad path parameter" (request "GET" "/users/not-an-int");
  run_case "bad query parameter"
    (request "GET" "/users/42" ~query:[ ("include_deleted", [ "sometimes" ]) ]);
  run_case "invalid json body"
    (request "POST" "/users" ~body:(`Assoc [ ("email", `Int 1) ]));
  run_case "invalid response body" (request "GET" "/users/2");
  run_case "invalid response status" (request "GET" "/users/500")
