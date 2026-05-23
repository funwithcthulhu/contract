type segment =
  | Static of string
  | Param of string

type t

val parse : string -> (t, Error.t) result
val raw : t -> string
val segments : t -> segment list
val match_path : t -> string -> ((string * string) list, Error.t) result
val to_openapi_path : t -> string
