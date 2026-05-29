(** Parsed route templates such as [/users/:id]. *)

type segment = Static of string | Param of string
type t

val parse : string -> (t, Error.t) result
(** Parse a route template. Templates must start with [/] and may not contain
    empty path segments. Parameter segments start with [:]. *)

val raw : t -> string
val segments : t -> segment list

val match_path : t -> string -> ((string * string) list, Error.t) result
(** Match a request path against a parsed template.

    Parameter values are percent-decoded after segment matching. Encoded slashes
    such as [%2F] therefore remain inside the parameter value; literal slashes
    still separate path segments. Static path segments are matched literally.
    Malformed percent escapes and invalid UTF-8 parameter values return [Error].
*)

val to_openapi_path : t -> string
