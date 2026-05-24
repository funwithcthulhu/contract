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

let path_param_error name ?expected ?got message =
  Error (Error.make ?expected ?got ~location:(Error.Path_param name) message)

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

let is_hex = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false

let hex_value = function
  | '0' .. '9' as ch -> Char.code ch - Char.code '0'
  | 'a' .. 'f' as ch -> 10 + Char.code ch - Char.code 'a'
  | 'A' .. 'F' as ch -> 10 + Char.code ch - Char.code 'A'
  | _ -> invalid_arg "hex_value"

let percent_decode_param name value =
  let length = String.length value in
  let buffer = Buffer.create length in
  let rec decode_at index =
    if index = length then Ok (Buffer.contents buffer)
    else
      match value.[index] with
      | '%' ->
          if
            index + 2 >= length
            || (not (is_hex value.[index + 1]))
            || not (is_hex value.[index + 2])
          then
            path_param_error name ~expected:"%HH" ~got:value
              "malformed percent escape"
          else (
            let code =
              (hex_value value.[index + 1] * 16) + hex_value value.[index + 2]
            in
            Buffer.add_char buffer (Char.chr code);
            decode_at (index + 3))
      | ch ->
          Buffer.add_char buffer ch;
          decode_at (index + 1)
  in
  decode_at 0

let match_path template path =
  let rec match_segments params segments parts =
    match (segments, parts) with
    | [], [] -> Ok (List.rev params)
    | Static expected :: rest_segments, got :: rest_parts
      when String.equal expected got ->
        match_segments params rest_segments rest_parts
    | Param name :: rest_segments, got :: rest_parts -> (
        match percent_decode_param name got with
        | Ok value ->
            match_segments ((name, value) :: params) rest_segments rest_parts
        | Error error -> Error error)
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
