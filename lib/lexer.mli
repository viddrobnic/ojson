type t

exception InvalidNumber of string
(** Raised when parsing an invalid number *)

exception InvalidIdentifier of string
(** Raised when parsing an invalid identifier such as true, false, null *)

exception InvalidChar of char
(** Raised when lexing an invalid character*)

val init : string -> t
(** Create a new lexer from an input *)

val next_token : t -> t * Token.t option
(** Moves the lexer to the next token.
    Returns a new lexer and an optional token.*)
