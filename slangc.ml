open Arg

let doLex = ref false;;
let doParse = ref false;;

let speclist = [
  ("-lex", Arg.Set doLex, "the compiler processes the input source file through the lexing phase, print the resulting stream of tokens to the console, and the exit");
  ("-parse", Arg.Set doParse, "the compiler processes the input source file through the parsing phase, print the resulting abstract syntax tree, and then exit.");
];;

let printUsage = "Usage: "^ Sys.argv.(0) ^" [-flags] [args]\n"^
                 "Available flags:";;

let driver (src:string) =
  let storeLex = src |> open_in |> Lexing.from_channel in
  if !doLex then
    "Unfortunately OCAML doesn't have a good way to print this, try the '-parse' flag" |> print_endline
  else if !doParse then
    storeLex
    |> Parser.prog Lexer.token
    |> Lang.exp_to_string
    |> print_endline
  else
    storeLex
    |> Parser.prog Lexer.token
    |> Lang.interpret
    |> Lang.string_of_value
    |> print_endline

let main () =
    (* Read the arguments *)
    Arg.parse
      speclist
      driver
      printUsage;;

let _ = if !Sys.interactive then () else main ()
  ;;
