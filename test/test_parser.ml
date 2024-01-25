open Ojson

let pp_object fmt t = Stdlib.Format.fprintf fmt "%s" (Object.to_string t)
let equal_object = Object.equal
let testable_object = Alcotest.testable pp_object equal_object

let test_parser input expected () =
  let lexer = Lexer.init input in
  let actual = Parser.parse lexer in
  Alcotest.(check testable_object) "same object" expected actual

let parse input () =
  let lexer = Lexer.init input in
  let _ = Parser.parse lexer in
  ()

let test_parser_exception input expected () =
  Alcotest.check_raises "test parser exception" (Failure expected) (parse input)

let () =
  let open Alcotest in
  let open Object in
  run "Parser"
    [
      ( "basic objects",
        [
          test_case "boolean true" `Quick (test_parser "true" (Bool true));
          test_case "boolean false" `Quick (test_parser "false" (Bool false));
          test_case "int" `Quick (test_parser "123" (Int 123));
          test_case "float" `Quick (test_parser "-123.12" (Float (-123.12)));
          test_case "string" `Quick (test_parser "\"asdf\"" (String "asdf"));
        ] );
      ( "list",
        [
          test_case "empty list" `Quick (test_parser "[]" (List []));
          test_case "singleton" `Quick (test_parser "[1]" (List [ Int 1 ]));
          test_case "two elements" `Quick
            (test_parser "[1, 2]" (List [ Int 1; Int 2 ]));
          test_case "required comma" `Quick
            (test_parser_exception "[1 2]" "Expected ',' or ']'");
          test_case "trailing comma fails" `Quick
            (test_parser_exception "[1, 2,]" "Unexpected token: Token.RightBox");
        ] );
      ( "object",
        [
          test_case "empty object" `Quick
            (test_parser "{}" (Object StrMap.empty));
          test_case "singleton" `Quick
            (test_parser "{\"a\": 10}" (Object (StrMap.singleton "a" (Int 10))));
          test_case "two elements" `Quick
            (test_parser "{\"a\": 10, \"b\": 20}"
               (Object (StrMap.of_list [ ("a", Int 10); ("b", Int 20) ])));
          test_case "three elements" `Quick
            (test_parser "{\"a\": 10, \"b\": 20, \"c\": 30}"
               (Object
                  (StrMap.of_list
                     [ ("a", Int 10); ("b", Int 20); ("c", Int 30) ])));
          test_case "required comma" `Quick
            (test_parser_exception "{\"a\": 10 \"b\": 20}" "Expected ',' or '}'");
          test_case "trailing comma fails" `Quick
            (test_parser_exception "{\"a\": 10, \"b\": 20,}"
               "Expect string as key");
        ] );
    ]
