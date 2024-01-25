open Ojson
open Base

let pp_token fmt t = Stdlib.Format.fprintf fmt "%s" (Token.show t)
let equal_token = Token.equal
let testable_token = Alcotest.testable pp_token equal_token

let test_lexer () =
  let open Token in
  let input = "[{:,\"test_string\"  \t \n 1 1.2 true false null  }] " in
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
      Bool true;
      Bool false;
      Null;
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

let get_token input () =
  let lexer = Lexer.init input in
  let _ = Lexer.next_token lexer in
  ()

let test_fail_number number () =
  Alcotest.check_raises "test fail number" (Lexer.InvalidNumber number)
    (get_token number)

let test_fail_identifier identifier () =
  Alcotest.check_raises "test fail identifier"
    (Lexer.InvalidIdentifier identifier) (get_token identifier)

let test_fail_invalid_char input ch () =
  Alcotest.check_raises "test fail invalid char" (Lexer.InvalidChar ch)
    (get_token input)

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
          (* fail cases *)
          test_case "fail float" `Quick (test_fail_number "10.1.2");
          test_case "fail negative float" `Quick (test_fail_number "-10.1.2");
          test_case "fail invalid minus signs" `Quick (test_fail_number "-10-");
          test_case "fail invalid minus signs 2" `Quick
            (test_fail_number "-10-123");
          test_case "fail invalid minus sings 3" `Quick
            (test_fail_number "10-123");
          test_case "fail invalid minus sings 4" `Quick (test_fail_number "-");
        ] );
      ( "identifier errors",
        [ test_case "fail identifier" `Quick (test_fail_identifier "test") ] );
      ( "invalid char",
        [
          test_case "fail invalid letter" `Quick
            (test_fail_invalid_char "A" 'A');
          test_case "fail invalid special char" `Quick
            (test_fail_invalid_char "!asdf" '!');
        ] );
    ]
