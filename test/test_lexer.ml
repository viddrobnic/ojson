open Ojson

let pp_token fmt t = Format.fprintf fmt "%s" (Token.show t)
let equal_token = Token.equal
let testable_token = Alcotest.testable pp_token equal_token

let test_lexer () =
  let open Token in
  let input = "[{:,\"test_string\"  \t \n 1 1.2  }] " in
  let lexer = Lexer.init input in
  let rec collect lexer acc =
    let lexer, tk = Lexer.next_token lexer in
    match tk with Some tk -> collect lexer (tk :: acc) | None -> List.rev acc
  in
  let tokens = collect lexer [] in
  Alcotest.(check (list testable_token))
    "test lexer"
    [
      LeftBox;
      LeftSwirly;
      Colon;
      Comma;
      String "test_string";
      Int 1;
      Number 1.2;
      RightSwirly;
      RightBox;
    ]
    tokens

let test_token input expected () =
  let lexer = Lexer.init input in
  let _, token = Lexer.next_token lexer in
  match token with
  | Some token -> Alcotest.(check testable_token) "test token" expected token
  | None -> Alcotest.fail "no token"

let () =
  let open Alcotest in
  run "Lexer"
    [
      ("lexer", [ test_case "test lexer" `Quick test_lexer ]);
      ( "number lexing",
        [
          (* ints *)
          test_case "positive int" `Quick (test_token "10" (Int 10));
          test_case "int zero" `Quick (test_token "0" (Int 0));
          test_case "negative int" `Quick (test_token "-123" (Int (-123)));
          test_case "negative int zero" `Quick (test_token "-0" (Int 0));
          (* floats *)
          test_case "positive float" `Quick
            (test_token "10.123" (Number 10.123));
          test_case "float zero" `Quick (test_token "0.0" (Number 0.0));
          test_case "negative float" `Quick
            (test_token "-123.45" (Number (-123.45)));
          test_case "negative float zero" `Quick
            (test_token "-0.0" (Number 0.0));
        ] );
    ]
