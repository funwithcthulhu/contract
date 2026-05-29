type segment = Static of string | Param of string
type t = { raw : string; segments : segment list }

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
    | _ ->
        Error (Error.make ~location:Error.Route ~got:path "empty path segment")

let parse_segment = function
  | "" -> route_error "empty path segment"
  | segment when segment.[0] = ':' ->
      let name = String.sub segment 1 (String.length segment - 1) in
      if name = "" then route_error "empty path parameter" else Ok (Param name)
  | segment -> Ok (Static segment)

let parse raw =
  match split_path raw with
  | Error error -> Error error
  | Ok parts ->
      let rec parse_all seen acc = function
        | [] -> Ok { raw; segments = List.rev acc }
        | part :: rest -> (
            match parse_segment part with
            | Ok (Param name as segment) ->
                if List.mem name seen then
                  route_error ("duplicate path parameter: " ^ name)
                else parse_all (name :: seen) (segment :: acc) rest
            | Ok segment -> parse_all seen (segment :: acc) rest
            | Error error -> Error error)
      in
      parse_all [] [] parts

let is_hex = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false

let hex_value = function
  | '0' .. '9' as ch -> Char.code ch - Char.code '0'
  | 'a' .. 'f' as ch -> 10 + Char.code ch - Char.code 'a'
  | 'A' .. 'F' as ch -> 10 + Char.code ch - Char.code 'A'
  | _ -> invalid_arg "hex_value"

let is_valid_utf8 value =
  let length = String.length value in
  let byte index = Char.code value.[index] in
  let byte_in_range index low high =
    index < length
    &&
    let value = byte index in
    value >= low && value <= high
  in
  let continuation index = byte_in_range index 0x80 0xbf in
  let rec valid_at index =
    if index = length then true
    else
      let first = byte index in
      if first <= 0x7f then valid_at (index + 1)
      else if first >= 0xc2 && first <= 0xdf then
        continuation (index + 1) && valid_at (index + 2)
      else if first = 0xe0 then
        byte_in_range (index + 1) 0xa0 0xbf
        && continuation (index + 2)
        && valid_at (index + 3)
      else if first >= 0xe1 && first <= 0xec then
        continuation (index + 1)
        && continuation (index + 2)
        && valid_at (index + 3)
      else if first = 0xed then
        byte_in_range (index + 1) 0x80 0x9f
        && continuation (index + 2)
        && valid_at (index + 3)
      else if first >= 0xee && first <= 0xef then
        continuation (index + 1)
        && continuation (index + 2)
        && valid_at (index + 3)
      else if first = 0xf0 then
        byte_in_range (index + 1) 0x90 0xbf
        && continuation (index + 2)
        && continuation (index + 3)
        && valid_at (index + 4)
      else if first >= 0xf1 && first <= 0xf3 then
        continuation (index + 1)
        && continuation (index + 2)
        && continuation (index + 3)
        && valid_at (index + 4)
      else if first = 0xf4 then
        byte_in_range (index + 1) 0x80 0x8f
        && continuation (index + 2)
        && continuation (index + 3)
        && valid_at (index + 4)
      else false
  in
  valid_at 0

let percent_decode_param name value =
  let length = String.length value in
  let buffer = Buffer.create length in
  let rec decode_at index =
    if index = length then
      let decoded = Buffer.contents buffer in
      if is_valid_utf8 decoded then Ok decoded
      else
        path_param_error name ~expected:"valid UTF-8" ~got:value
          "invalid UTF-8 path parameter"
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
          else
            let code =
              (hex_value value.[index + 1] * 16) + hex_value value.[index + 2]
            in
            Buffer.add_char buffer (Char.chr code);
            decode_at (index + 3)
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
