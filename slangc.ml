open Arg
open Printf

let length = ref false;;

let speclist = [
    ("-length", Arg.Set length, "prints the lengths of each of the arguments");
  ];;

(*
 * Prints the arguments, one per line, if no flags present, else prints the length of arguments one per line if "-length" is present
 *)
let printerSpool (p:string)=
  if !length then
    printf "%i\n" (String.length p)
  else
    printf "%s\n" p;;

let printUsage = "Usage: "^ Sys.argv.(0) ^" [-flags] [args]\n"^
                 "Available flags:";;

(*
 * Main function, uses the Arg package to parse arguments
 *)
let () =
  (* Read the arguments *)
  Arg.parse
    speclist
    printerSpool
    printUsage;;
