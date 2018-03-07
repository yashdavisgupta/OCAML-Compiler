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
  ; ("/", SLASH)
  ; ("+.", FPLUS)
  ; ("-.", FMINUS)
  ; ("*.", FTIMES)
  ; ("/.", FSLASH)
  ]

let create_symbol lexbuf =
  let str = lexeme lexbuf in
  List.assoc str symbols

let create_int lexbuf = lexeme lexbuf |> int_of_string
}

let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?
let int = '-'? ['0'-'9'] ['0'-'9']*

let symbol     = '(' | ')' | '+' | '-' | '*' | '/' | "+." | "-." | "*." | "/."
let newline    = '\n' | ('\r' '\n') | '\r'
let whitespace = ['\t' ' ']

rule token = parse
  | eof                       { EOF }
  | int                       { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float                     { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | whitespace+ | newline+    { token lexbuf }
  | symbol                    { create_symbol lexbuf }
  | "true"                    { TRUE }
  | "false"                   { FALSE }
  | "if"                      { IF }
  | "<="                      { LESSEQ }
  | "NaN"                     { FLOAT nan }
  | _ as c { raise @@ Lexer_error ("Unexpected character: " ^ Char.escaped c) }
