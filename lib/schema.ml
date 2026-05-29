type primitive = String | Integer | Number | Boolean

type t =
  | Any
  | Null
  | Primitive of primitive
  | Array of t
  | Object of (string * t * bool) list
  | Enum of string list

let string = Primitive String
let integer = Primitive Integer
let number = Primitive Number
let boolean = Primitive Boolean
let array item = Array item
let obj fields = Object fields
let enum values = Enum values

let primitive_type = function
  | String -> "string"
  | Integer -> "integer"
  | Number -> "number"
  | Boolean -> "boolean"

let rec to_openapi = function
  | Any -> `Assoc []
  | Null -> `Assoc [ ("nullable", `Bool true) ]
  | Primitive primitive ->
      `Assoc [ ("type", `String (primitive_type primitive)) ]
  | Array item ->
      `Assoc [ ("type", `String "array"); ("items", to_openapi item) ]
  | Object fields ->
      let properties =
        fields |> List.map (fun (name, schema, _) -> (name, to_openapi schema))
      in
      let required =
        fields
        |> List.filter_map (fun (name, _, is_required) ->
            if is_required then Some (`String name) else None)
      in
      let base =
        [ ("type", `String "object"); ("properties", `Assoc properties) ]
      in
      let fields =
        match required with
        | [] -> base
        | _ -> base @ [ ("required", `List required) ]
      in
      `Assoc fields
  | Enum values ->
      `Assoc
        [
          ("type", `String "string");
          ("enum", `List (List.map (fun value -> `String value) values));
        ]
