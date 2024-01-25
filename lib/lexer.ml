open Base

type t = { input : string; position : int; ch : char option }

exception InvalidNumber of string
exception InvalidIdentifier of string
exception InvalidChar of char

let init input =
  if String.is_empty input then { input; position = 0; ch = None }
  else { input; position = 0; ch = Some input.[0] }

let advance lexer =
  if lexer.position >= String.length lexer.input - 1 then
    { lexer with position = String.length lexer.input; ch = None }
  else
    let position = lexer.position + 1 in
    { lexer with position; ch = Some lexer.input.[position] }

let rec skip_whitespace lexer =
  match lexer.ch with
  | Some ch when Char.is_whitespace ch -> skip_whitespace (advance lexer)
  | _ -> lexer

let seek lexer condition =
  let rec loop lexer =
    if condition lexer.ch then loop (advance lexer) else lexer
  in
  let lexer = loop lexer in
  (lexer, lexer.position)

let read_while lexer condition =
  let start_pos = lexer.position in
  let lexer, end_pos =
    seek lexer (fun ch ->
        match ch with Some ch -> condition ch | None -> false)
  in
  (lexer, String.sub lexer.input ~pos:start_pos ~len:(end_pos - start_pos))

let read_string lexer =
  let lexer = advance lexer in
  let lexer, str = read_while lexer (fun ch -> Char.(ch <> '"')) in
  (advance lexer, Token.String str)

let is_number ch = Char.is_digit ch || Char.(ch = '.') || Char.(ch = '-')

let read_number lexer =
  let lexer, str = read_while lexer is_number in
  let token =
    try
      if String.contains str '.' then Token.Number (Float.of_string str)
      else Token.Int (Int.of_string str)
    with _ -> raise (InvalidNumber str)
  in
  (lexer, token)

let read_constant lexer =
  let lexer, str = read_while lexer Char.is_lowercase in
  let open Token in
  let token =
    match str with
    | "true" -> Bool true
    | "false" -> Bool false
    | "null" -> Null
    | _ -> raise (InvalidIdentifier str)
  in
  (lexer, token)

let next_token lexer =
  let open Token in
  let lexer = skip_whitespace lexer in
  match lexer.ch with
  | None -> (lexer, None)
  | Some ch ->
      let lexer, token =
        match ch with
        | '[' -> (advance lexer, LeftBox)
        | ']' -> (advance lexer, RightBox)
        | '{' -> (advance lexer, LeftSwirly)
        | '}' -> (advance lexer, RightSwirly)
        | ':' -> (advance lexer, Colon)
        | ',' -> (advance lexer, Comma)
        | '"' -> read_string lexer
        | ch when is_number ch -> read_number lexer
        | ch when Char.is_lowercase ch -> read_constant lexer
        | ch -> raise (InvalidChar ch)
      in
      (lexer, Some token)
