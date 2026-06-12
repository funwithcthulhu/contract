(** Pure API contract made from endpoint declarations. *)

type t

val make :
  title:string -> version:string -> Endpoint.t list -> (t, Error.t) result
(** Build an API contract.

    Exact duplicate method/path declarations are rejected. Ambiguous routes that
    differ only by parameter names are also rejected. Endpoint order is
    otherwise preserved for deterministic output. *)

val title : t -> string
val version : t -> string
val endpoints : t -> Endpoint.t list

val match_request : t -> Request.t -> (Endpoint.t, Error.t) result
(** Select the endpoint for a request without running parameter/body validation.

    Static segments are preferred over parameter segments. A request whose path
    matches an endpoint with a different method reports the allowed methods for
    that path. *)
