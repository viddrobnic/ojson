type t =
  | LeftBox
  | RightBox
  | LeftSwirly
  | RightSwirly
  | Colon
  | Comma
  | String of string
  | Bool of bool
  | Int of int
  | Number of float
  | Null
[@@deriving show, eq]
