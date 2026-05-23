open Contract

type user = {
  id : int;
  email : string;
  name : string option;
}

type create_user = {
  email : string;
  name : string option;
}

let ( let* ) = Result.bind

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
  Schema.obj
    [
      ("email", Schema.string, true);
      ("name", Schema.string, false);
    ]

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
  Json_codec.make ~name:"CreateUser" ~schema:create_user_schema ~encode ~decode ()

let get_user =
  Endpoint.get ~summary:"Fetch a user" ~operation_id:"getUser" "/users/:id"
  |> Endpoint.path_param "id" Codec.int
  |> Endpoint.query_param "include_deleted" Codec.bool
  |> Endpoint.response ~status:200 user_codec

let create_user =
  Endpoint.post ~summary:"Create a user" ~operation_id:"createUser" "/users"
  |> Endpoint.body create_user_codec
  |> Endpoint.response ~status:201 user_codec

let api : Openapi.api =
  {
    title = "Users API";
    version = "0.1.0";
    endpoints = [ get_user; create_user ];
  }

let () =
  print_endline (Openapi.to_string ~pretty:true api)
