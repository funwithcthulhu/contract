type t = { title : string; version : string; endpoints : Endpoint.t list }

let title api = api.title
let version api = api.version
let endpoints api = api.endpoints

let endpoint_label endpoint =
  Endpoint.method_to_string endpoint.Endpoint.method_
  ^ " "
  ^ Path_template.raw endpoint.path

let route_shape endpoint =
  endpoint.Endpoint.path |> Path_template.segments
  |> List.map (function
    | Path_template.Static segment -> "static:" ^ segment
    | Param _ -> "param")

let same_method left right = left.Endpoint.method_ = right.Endpoint.method_

let duplicate_route left right =
  same_method left right
  && String.equal
       (Path_template.raw left.Endpoint.path)
       (Path_template.raw right.Endpoint.path)

let ambiguous_route left right =
  same_method left right
  && (not
        (String.equal
           (Path_template.raw left.Endpoint.path)
           (Path_template.raw right.Endpoint.path)))
  && route_shape left = route_shape right

let duplicate_error endpoint =
  Error.make ~location:Error.Route ~got:(endpoint_label endpoint)
    "duplicate endpoint declaration"

let ambiguous_error left right =
  Error.make ~location:Error.Route
    ~got:(endpoint_label left ^ " conflicts with " ^ endpoint_label right)
    "ambiguous endpoint route"

let validate_routes endpoints =
  let rec validate seen = function
    | [] -> Ok ()
    | endpoint :: rest -> (
        match List.find_opt (duplicate_route endpoint) seen with
        | Some _ -> Error (duplicate_error endpoint)
        | None -> (
            match List.find_opt (ambiguous_route endpoint) seen with
            | Some conflicting -> Error (ambiguous_error conflicting endpoint)
            | None -> validate (endpoint :: seen) rest))
  in
  validate [] endpoints

let make ~title ~version endpoints =
  match validate_routes endpoints with
  | Ok () -> Ok { title; version; endpoints }
  | Error error -> Error error

let static_count endpoint =
  endpoint.Endpoint.path |> Path_template.segments
  |> List.fold_left
       (fun count -> function
         | Path_template.Static _ -> count + 1 | Param _ -> count)
       0

let plain_route_mismatch error =
  error.Error.location = Error.Route
  && String.equal error.message "path does not match route"

let first_relevant_error errors =
  List.find_opt (fun error -> not (plain_route_mismatch error)) errors

let matching_endpoints endpoints method_ path =
  let rec collect matches errors = function
    | [] -> (List.rev matches, List.rev errors)
    | endpoint :: rest when endpoint.Endpoint.method_ <> method_ ->
        collect matches errors rest
    | endpoint :: rest -> (
        match Path_template.match_path endpoint.path path with
        | Ok _ -> collect (endpoint :: matches) errors rest
        | Error error -> collect matches (error :: errors) rest)
  in
  collect [] [] endpoints

let best_match = function
  | [] -> None
  | first :: rest ->
      let rec choose best best_score tied = function
        | [] -> Some (best, tied)
        | endpoint :: rest ->
            let score = static_count endpoint in
            if score > best_score then choose endpoint score [] rest
            else if score = best_score then
              choose best best_score (endpoint :: tied) rest
            else choose best best_score tied rest
      in
      choose first (static_count first) [] rest

let ambiguous_match_error endpoint tied =
  let labels = endpoint :: tied |> List.map endpoint_label in
  Error.make ~location:Error.Route
    ~got:(String.concat " conflicts with " labels)
    "ambiguous endpoint match"

let unique_methods endpoints =
  let add methods endpoint =
    let method_ = Endpoint.method_to_string endpoint.Endpoint.method_ in
    if List.mem method_ methods then methods else methods @ [ method_ ]
  in
  List.fold_left add [] endpoints

let endpoints_matching_path endpoints path =
  endpoints
  |> List.filter (fun endpoint ->
      match Path_template.match_path endpoint.Endpoint.path path with
      | Ok _ -> true
      | Error _ -> false)

let method_mismatch request methods =
  Error.make ~location:Error.Method
    ~expected:(String.concat ", " methods)
    ~got:(Endpoint.method_to_string request.Request.method_)
    "HTTP method does not match any endpoint for path"

let route_mismatch request =
  Error.make ~location:Error.Route ~got:request.Request.path
    "path does not match any endpoint"

let match_request api request =
  let matches, errors =
    matching_endpoints api.endpoints request.Request.method_ request.path
  in
  match best_match matches with
  | Some (endpoint, []) -> Ok endpoint
  | Some (endpoint, tied) -> Error (ambiguous_match_error endpoint tied)
  | None -> (
      match first_relevant_error errors with
      | Some error -> Error error
      | None -> (
          let allowed =
            endpoints_matching_path api.endpoints request.path |> unique_methods
          in
          match allowed with
          | [] -> Error (route_mismatch request)
          | methods -> Error (method_mismatch request methods)))
