# OJson

A (very) simple JSON parser written in OCaml. This a learning project to practice my
OCaml and parsing skills, therefore it shouldn't be used in production :smile:.

## Usage

Basic usage is

```ocaml
open Ojson

let lexer = Lexer.init input in
let obj = Parser.parse lexer in
match obj with
    | Object.Null -> print_endline "got null"
    | Object.String str -> print_endline ("got string: " ^ str)
    | _ -> print_endline "..."
```
