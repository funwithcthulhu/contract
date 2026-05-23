type segment =
  | Static of string
  | Param of string

type t = {
  raw : string;
  segments : segment list;
}

let raw template = template.raw
let segments template = template.segments

let route_error ?got message =
  Error (Error.make ?got ~location:Error.Route message)

let split_path path =
  if String.length path = 0 || path.[0] <> '/' then
    Error (Error.make ~location:Error.Route ~got:path "path must start with /")
  else
    match String.split_on_char '/' path with
    | "" :: [ "" ] -> Ok []
    | "" :: parts when List.exists (( = ) "") parts ->
        Error (Error.make ~location:Error.Route ~got:path "empty path segment")
    | "" :: parts -> Ok parts
    | _ -> Error (Error.make ~location:Error.Route ~got:path "empty path segment")

let parse_segment = function
  | "" -> route_error "empty path segment"
  | segment when segment.[0] = ':' ->
      let name = String.sub segment 1 (String.length segment - 1) in
      if name = "" then route_error "empty path parameter"
      else Ok (Param name)
  | segment -> Ok (Static segment)

let parse raw =
  match split_path raw with
  | Error error -> Error error
  | Ok parts ->
      let rec parse_all acc = function
        | [] -> Ok { raw; segments = List.rev acc }
        | part :: rest -> (
            match parse_segment part with
            | Ok segment -> parse_all (segment :: acc) rest
            | Error error -> Error error)
      in
      parse_all [] parts

let match_path template path =
  let rec match_segments params segments parts =
    match (segments, parts) with
    | [], [] -> Ok (List.rev params)
    | Static expected :: rest_segments, got :: rest_parts
      when String.equal expected got ->
        match_segments params rest_segments rest_parts
    | Param name :: rest_segments, got :: rest_parts ->
        match_segments ((name, got) :: params) rest_segments rest_parts
    | _ -> route_error ~got:path "path does not match route"
  in
  match split_path path with
  | Error error -> Error error
  | Ok parts -> match_segments [] template.segments parts

let segment_to_openapi = function
  | Static segment -> segment
  | Param name -> "{" ^ name ^ "}"

let to_openapi_path template =
  "/" ^ String.concat "/" (List.map segment_to_openapi template.segments)
