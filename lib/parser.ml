let rec parse_aux lexer =
  let open Token in
  let lexer, token = Lexer.next_token lexer in
  let token =
    match token with None -> failwith "No tokens in lexer" | Some tk -> tk
  in
  match token with
  | Null -> (lexer, Object.Null)
  | Bool b -> (lexer, Object.Bool b)
  | String s -> (lexer, Object.String s)
  | Int i -> (lexer, Object.Int i)
  | Number f -> (lexer, Object.Float f)
  | LeftBox ->
      let next_lexer, next_token = Lexer.next_token lexer in
      if next_token = Some RightBox then (next_lexer, Object.List [])
      else
        let lexer, obj = parse_aux lexer in
        parse_list lexer [ obj ]
  | LeftSwirly ->
      let open Object in
      let next_lexer, next_token = Lexer.next_token lexer in
      if next_token = Some RightSwirly then (next_lexer, Object StrMap.empty)
      else
        let lexer, key, value = parse_object_entry lexer in
        parse_object lexer (StrMap.singleton key value)
  | _ -> failwith ("Unexpected token: " ^ Token.show token)

and parse_list lexer lst =
  let open Token in
  let lexer, token = Lexer.next_token lexer in
  let token =
    match token with None -> failwith "Failed to parse list" | Some tk -> tk
  in
  match token with
  | RightBox -> (lexer, Object.List (List.rev lst))
  | Comma ->
      let lexer, obj = parse_aux lexer in
      parse_list lexer (obj :: lst)
  | _ -> failwith "Expected ',' or ']'"

and parse_object_entry lexer =
  let lexer, key = Lexer.next_token lexer in
  let key =
    match key with
    | Some (Token.String s) -> s
    | _ -> failwith "Expect string as key"
  in

  let lexer, colon = Lexer.next_token lexer in
  if colon <> Some Token.Colon then failwith "Expect ':' after key"
  else
    let lexer, value = parse_aux lexer in
    (lexer, key, value)

and parse_object lexer map =
  let open Token in
  let lexer, token = Lexer.next_token lexer in
  match token with
  | None -> failwith "Failed to parse object"
  | Some RightSwirly -> (lexer, Object.Object map)
  | Some Comma ->
      let lexer, key, value = parse_object_entry lexer in
      parse_object lexer (Object.StrMap.add key value map)
  | _ -> failwith "Expected ',' or '}'"

let parse lexer = snd (parse_aux lexer)
