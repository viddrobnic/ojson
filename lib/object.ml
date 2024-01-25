module StrMap = Map.Make (String)

type t =
  | Null
  | Bool of bool
  | String of string
  | Int of int
  | Float of float
  | List of t list
  | Object of t StrMap.t
[@@deriving eq]

let rec to_string = function
  | Null -> "null"
  | Bool b -> string_of_bool b
  | String s -> "\"" ^ s ^ "\""
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | List l -> "[" ^ String.concat ", " (List.map to_string l) ^ "]"
  | Object obj ->
      "{"
      ^ String.concat ", "
          (StrMap.fold (fun k v acc -> (k ^ ": " ^ to_string v) :: acc) obj [])
      ^ "}"
