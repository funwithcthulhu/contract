(** Small schema AST used for OpenAPI output.

    Object fields use [(name, schema, required)]. This is intentionally smaller
    than full JSON Schema. *)

type primitive =
  | String
  | Integer
  | Number
  | Boolean

type t =
  | Any
  | Null
  | Primitive of primitive
  | Array of t
  | Object of (string * t * bool) list
  | Enum of string list

val string : t
val integer : t
val number : t
val boolean : t
val array : t -> t
val obj : (string * t * bool) list -> t
val enum : string list -> t
val to_openapi : t -> Yojson.Safe.t
