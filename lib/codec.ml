type 'a t = {
  name : string;
  schema : Schema.t;
  encode : 'a -> string;
  decode : string -> ('a, Error.t) result;
}

let decode_error ~name value =
  Error
    (Error.make ~location:Error.Route ~expected:name ~got:value
       ("expected " ^ name))

let string =
  {
    name = "string";
    schema = Schema.string;
    encode = Fun.id;
    decode = (fun value -> Ok value);
  }

let int =
  {
    name = "integer";
    schema = Schema.integer;
    encode = string_of_int;
    decode =
      (fun value ->
        match int_of_string_opt value with
        | Some value -> Ok value
        | None -> decode_error ~name:"integer" value);
  }

let bool =
  {
    name = "boolean";
    schema = Schema.boolean;
    encode = string_of_bool;
    decode =
      (function
      | "true" -> Ok true
      | "false" -> Ok false
      | value -> decode_error ~name:"boolean" value);
  }

let float =
  {
    name = "number";
    schema = Schema.number;
    encode = string_of_float;
    decode =
      (fun value ->
        match float_of_string_opt value with
        | Some value -> Ok value
        | None -> decode_error ~name:"number" value);
  }
