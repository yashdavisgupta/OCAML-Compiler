{
open Lexing
open Parser

exception Lexer_error of string

let symbols : (string * Parser.token) list =
  [ ("(", LPAREN)
  ; (")", RPAREN)
  ; ("+", PLUS)
  ; ("-", MINUS)
  ; ("*", TIMES)
  ; ("/", DIVIDE)
  ; ("<=", LESSEQ)
  ; (">=", GREATEREQ)
  ; ("<", LESSTHAN)
  ; (">", GREATERTHAN)
  ; ("if", IF)
  ; ("then", THEN)
  ; ("else", ELSE)
  ; ("let", LET)
  ; ("=", EQUALS)
  ; ("in", IN)
  ; ("fun", FUN)
  ; ("->", ARROW)
  ; ("fix", FIX)
  ]

let create_symbol lexbuf =
  let str = lexeme lexbuf in
  List.assoc str symbols
}

let newline    = '\n' | ('\r' '\n') | '\r'
let whitespace = ['\t' ' ']
let digit      = ['0'-'9']
let var_name   = ['a'-'z'  'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '-' '_' ]*

let symbol = '(' | ')' | '+' | '-' | '*' | '/' | '>' | '<' | "<=" | ">=" | "if" | "then" | "else" | "let" | '=' | "in" | "fun" | "->" | "fix"

rule token = parse
  | eof                       { EOF }
  | digit+                    { INT (int_of_string (lexeme lexbuf)) }
  | whitespace+ | newline+    { token lexbuf }
  | symbol                    { create_symbol lexbuf }
  | "true" | "false"          { BOOL (bool_of_string (lexeme lexbuf)) }
  | var_name                  { NAME (lexeme lexbuf) }
  | _ as c { raise @@ Lexer_error ("Unexpected character: " ^ Char.escaped c) }
