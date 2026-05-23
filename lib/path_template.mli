(** Parsed route templates such as [/users/:id]. *)

type segment =
  | Static of string
  | Param of string

type t

(** Parse a route template. Templates must start with [/] and may not contain
    empty path segments. Parameter segments start with [:]. *)
val parse : string -> (t, Error.t) result

val raw : t -> string
val segments : t -> segment list

(** Match a request path against a parsed template. Returned parameter values
    are raw path segment strings; this version does not percent-decode them. *)
val match_path : t -> string -> ((string * string) list, Error.t) result

val to_openapi_path : t -> string
