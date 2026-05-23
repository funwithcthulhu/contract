type location =
  | Method
  | Route
  | Path_param of string
  | Query_param of string
  | Body
  | Json_field of string

type t = {
  location : location;
  message : string;
  expected : string option;
  got : string option;
}

let make ?expected ?got ~location message =
  { location; message; expected; got }

let location_to_string = function
  | Method -> "method"
  | Route -> "route"
  | Path_param name -> "path parameter " ^ name
  | Query_param name -> "query parameter " ^ name
  | Body -> "body"
  | Json_field name -> "json field " ^ name

let append_option label = function
  | None -> []
  | Some value -> [ label ^ ": " ^ value ]

let to_string error =
  let details =
    append_option "expected" error.expected @ append_option "got" error.got
  in
  match details with
  | [] -> location_to_string error.location ^ ": " ^ error.message
  | _ ->
      location_to_string error.location ^ ": " ^ error.message ^ " ("
      ^ String.concat ", " details
      ^ ")"
