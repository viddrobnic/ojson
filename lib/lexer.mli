type t

val init : string -> t
(** Create a new lexer from an input *)

val next_token : t -> t * Token.t option
(** Moves the lexer to the next token.
    Returns a new lexer and an optional token.*)
