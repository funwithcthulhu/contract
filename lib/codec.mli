(** Scalar codecs for path and query parameters. *)

type 'a t = {
  name : string;
  schema : Schema.t;
  encode : 'a -> string;
  decode : string -> ('a, Error.t) result;
}

(** Accepts any input and returns it unchanged. *)
val string : string t

(** Decodes with [int_of_string_opt]. *)
val int : int t

(** Accepts only ["true"] and ["false"]. *)
val bool : bool t

(** Decodes with [float_of_string_opt]. *)
val float : float t
