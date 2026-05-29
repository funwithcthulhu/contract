(** Scalar codecs for path and query parameters. *)

type 'a t = {
  name : string;
  schema : Schema.t;
  encode : 'a -> string;
  decode : string -> ('a, Error.t) result;
}

val string : string t
(** Accepts any input and returns it unchanged. *)

val int : int t
(** Decodes with [int_of_string_opt]. *)

val bool : bool t
(** Accepts only ["true"] and ["false"]. *)

val float : float t
(** Decodes with [float_of_string_opt]. *)
