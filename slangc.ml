open Pervasives
open Lexer
open Parser
open Lang

let doLex = ref false;;
let doParse = ref false;;
let doStep = ref false;;

let speclist = [
  ("-lex", Arg.Set doLex, "the compiler processes the input source file through the lexing phase, prints the resulting stream of tokens to the console, and the exits");
  ("-parse", Arg.Set doParse, "the compiler processes the input source file through the parsing phase, prints the resulting abstract syntax tree, and then exits.");
  ("-step", Arg.Set doStep, "the compiler processes the input source file executing the program step by step, prints the resulting steps, and then exits.")
];;

let printUsage = "Usage: "^ Sys.argv.(0) ^" [-flags] [args]\n"^
                 "Available flags:";;

let driver (src:string) =
  let storeLex = open_in src |> Lexing.from_channel in
  if !doLex then
    "Unfortunately OCAML doesn't have a good way to print this, try the '-parse' flag" |> print_endline
  else if !doParse then
    storeLex
    |> Parser.prog token
    |> exp_to_string'
    |> print_endline
  else if !doStep then
    storeLex
    |> Parser.prog token
    |> eval_ret
  else
    storeLex
    |> Parser.prog token
    |> eval
    |> value_to_string'
    |> print_endline

let main () =
  Arg.parse
    speclist
    driver
    printUsage;;

  let _ = if !Sys.interactive then () else main ()
    ;;
