type 'a t = {
  name : string option;
  schema : Schema.t;
  encode : 'a -> Yojson.Safe.t;
  decode : Yojson.Safe.t -> ('a, Error.t) result;
}

let make ?name ~schema ~encode ~decode () = { name; schema; encode; decode }

let json_type = function
  | `Null -> "null"
  | `Bool _ -> "boolean"
  | `Int _ -> "integer"
  | `Intlit _ -> "integer"
  | `Float _ -> "number"
  | `String _ -> "string"
  | `Assoc _ -> "object"
  | `List _ -> "array"
  | `Tuple _ -> "tuple"
  | `Variant _ -> "variant"

let decode_error ~expected json =
  Error
    (Error.make ~location:Error.Body ~expected ~got:(json_type json)
       ("expected " ^ expected))

let string =
  make ~schema:Schema.string
    ~encode:(fun value -> `String value)
    ~decode:(function
      | `String value -> Ok value | json -> decode_error ~expected:"string" json)
    ()

let int =
  make ~schema:Schema.integer
    ~encode:(fun value -> `Int value)
    ~decode:(function
      | `Int value -> Ok value | json -> decode_error ~expected:"integer" json)
    ()

let bool =
  make ~schema:Schema.boolean
    ~encode:(fun value -> `Bool value)
    ~decode:(function
      | `Bool value -> Ok value | json -> decode_error ~expected:"boolean" json)
    ()

let float =
  make ~schema:Schema.number
    ~encode:(fun value -> `Float value)
    ~decode:(function
      | `Float value -> Ok value
      | `Int value -> Ok (float_of_int value)
      | json -> decode_error ~expected:"number" json)
    ()

let option codec =
  make ?name:codec.name ~schema:codec.schema
    ~encode:(function None -> `Null | Some value -> codec.encode value)
    ~decode:(function
      | `Null -> Ok None
      | json -> (
          match codec.decode json with
          | Ok value -> Ok (Some value)
          | Error error -> Error error))
    ()

let list codec =
  let rec decode_items acc = function
    | [] -> Ok (List.rev acc)
    | json :: rest -> (
        match codec.decode json with
        | Ok value -> decode_items (value :: acc) rest
        | Error error -> Error error)
  in
  make
    ~schema:(Schema.array codec.schema)
    ~encode:(fun values -> `List (List.map codec.encode values))
    ~decode:(function
      | `List values -> decode_items [] values
      | json -> decode_error ~expected:"array" json)
    ()

let field_error name message =
  Error (Error.make ~location:(Error.Json_field name) message)

let with_field name = function
  | Ok value -> Ok value
  | Error error -> Error { error with Error.location = Error.Json_field name }

let required_field name codec = function
  | `Assoc fields -> (
      match List.assoc_opt name fields with
      | Some json -> with_field name (codec.decode json)
      | None -> field_error name "missing required field")
  | json -> decode_error ~expected:"object" json

let optional_field name codec = function
  | `Assoc fields -> (
      match List.assoc_opt name fields with
      | None | Some `Null -> Ok None
      | Some json ->
          with_field name (codec.decode json)
          |> Result.map (fun value -> Some value))
  | json -> decode_error ~expected:"object" json
