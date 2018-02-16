type value =
  | Vint   of int
  | Vbool  of bool
  | Vfloat of float

type exp =
  | EInt  of int
  | EBool of bool
  | EFloat of float
  | ENaN  of float
  | EAdd  of exp * exp
  | ESub  of exp * exp
  | EMuti of exp * exp
  | EDivi of exp * exp
  | EFAdd  of exp * exp
  | EFSub  of exp * exp
  | EFMuti of exp * exp
  | EFDivi of exp * exp
  | EIf   of exp * exp * exp
  | ELessEq of exp * exp

let int_of_value (e:value) : int =
  match e with
  | Vint n -> n
  | _      -> failwith "Unexpected character found: Not an int"

let bool_of_value (e:value) : bool =
  match e with
  | Vbool n -> n
  | _       -> failwith "Unexpected character found: Not a bool"

let float_of_value (e:value) : float =
  match e with
  | Vfloat n -> n
  | _        -> failwith "Unexpected character found: Not a float"

let value_of_int (e:int) : value =
  match e with
  | int  -> Vint e

let value_of_bool (e:bool) : value =
  match e with
  | bool -> Vbool e

let value_of_float (e:float) : value =
  match e with
  | float -> Vfloat e

let string_of_value (e:value) : string =
  match e with
  | Vint n   -> string_of_int n
  | Vbool n  -> string_of_bool n
  | Vfloat n -> string_of_float n

let value_to_float (e:value) : float =
  match e with
  | Vint n   -> float_of_int n
  | Vfloat n -> n
  | _        -> failwith "to_float failed"

let rec interpret (e:exp) : value =
  match e with
  | EInt  n            -> Vint n
  | EBool b            -> Vbool b
  | EFloat f           -> Vfloat f
  | ENaN n             -> Vfloat nan
  | EAdd  (e1, e2)     -> value_of_int ((int_of_value (interpret e1)) + (int_of_value (interpret e2)))
  | ESub  (e1, e2)     -> value_of_int ((int_of_value (interpret e1)) - (int_of_value (interpret e2)))
  | EMuti (e1, e2)     -> value_of_int ((int_of_value (interpret e1)) * (int_of_value (interpret e2)))
  | EDivi (e1, e2)     -> value_of_int ((int_of_value (interpret e1)) / (int_of_value (interpret e2)))
  | EFAdd  (e1, e2)    -> value_of_float ((value_to_float (interpret e1)) +. (value_to_float (interpret e2)))
  | EFSub  (e1, e2)    -> value_of_float ((value_to_float (interpret e1)) -. (value_to_float (interpret e2)))
  | EFMuti (e1, e2)    -> value_of_float ((value_to_float (interpret e1)) *. (value_to_float (interpret e2)))
  | EFDivi (e1, e2)    -> value_of_float ((value_to_float (interpret e1)) /. (value_to_float (interpret e2)))
  | EIf   (e1, e2, e3) -> if bool_of_value (interpret e1) then interpret e2 else interpret e3
  | ELessEq (e1, e2)   -> value_of_bool (value_to_float (interpret e1) <= value_to_float (interpret e2))

type token =
  | TInt of int
  | TFloat of float
  | TLParen
  | TRParen
  | TPlus
  | TMinus
  | TTimes
  | TSlash
  | TFPlus
  | TFMinus
  | TFTimes
  | TFSlash
  | TIf
  | TLessEq
  | TFalse of bool
  | TTrue of bool
  | TNAN

let string_of_token (t:token) : string =
  match t with
  | TInt n   -> string_of_int n
  | TFloat n -> string_of_float n
  | TFalse n -> "false"
  | TTrue n  -> "true"
  | TNAN     -> "NaN"
  | TLParen  -> "("
  | TRParen  -> ")"
  | TPlus    -> "+"
  | TMinus   -> "-"
  | TTimes   -> "*"
  | TSlash   -> "/"
  | TFPlus    -> "+."
  | TFMinus   -> "-."
  | TFTimes   -> "*."
  | TFSlash   -> "/."
  | TIf      -> "if"
  | TLessEq  -> "<="

let string_of_token_list (toks:token list) : string =
  String.concat "," (List.map string_of_token toks)

(* Peeks at the head of the stream without advancing it forward *)
let peek (src:char Stream.t) : char =
  match Stream.peek src with
  | Some ch -> ch
  | None    -> failwith "Unexpected end of file encountered"

(* Pops the head of the stream and returns it, advancing the stream forward *)
let advance : char Stream.t -> char = Stream.next

let advanceto (n:int) (src:char Stream.t) : char Stream.t -> char =
for i = 1 to n do
  Stream.next src |> ignore;
done;
Stream.next

(* Returns true iff this stream still has elements left *)
let is_empty (src:char Stream.t) : bool =
  try
    Stream.empty src; true
  with
    Stream.Failure -> false

let is_whitespace (ch:char) : bool =
  ch = ' ' || ch = '\012' || ch = '\n' || ch = '\r' || ch = '\t'

let is_digit (ch:char) : bool =
  let code = Char.code ch in
  48 <= code && code <= 57

let is_alpha (ch:char) : bool =
  let code = Char.code ch in
  (65 <= code && code <= 90) || (97 <= code && code <= 122)

let lex_word (str:string) (src:char Stream.t) : bool =
  let length = String.length str in
  let ch = Stream.npeek length src in
  let str1 = (let buf = Buffer.create length in
  List.iter (Buffer.add_char buf) ch;
  Buffer.contents buf) in
  String.equal str str1

(* Note: lex contains two nested helper functions, lex_num and go *)
let lex (src:char Stream.t) : token list =
  let rec lex_num acc: string =
    if is_digit (peek src) then
      lex_num (acc ^ (Char.escaped (advance src)))
    else if peek src == '.' then
      lex_num (acc ^ (Char.escaped (advance src)))
    else
      acc
  in
  let rec go () =
    if not (is_empty src) then
      let ch = peek src in
       if is_alpha ch then
         (if (lex_word "if" src) then
            (advanceto 2 src |> ignore; TIf :: go ())
          else if lex_word "false" src then
            (advanceto 5 src |> ignore; TFalse false :: go ())
          else if (lex_word "true" src) then
            (advanceto 4 src |> ignore; TTrue true :: go ())
          else if (lex_word "NaN" src) then
            (advanceto 3 src |> ignore; TNAN :: go ())
          else failwith (Printf.sprintf "Unexpected character found: %c" ch))
      else
      (* Note: the |> operator takes the result of the left-hand side
       * and feeds it as an argument to the function on the right-hand
       * side.  ignore has type 'a -> unit---it allows us to throw
       * away the return type of a function we don't care about *)
      match ch with
      | '('  -> advance src |> ignore; TLParen :: go ()
      | ')'  -> advance src |> ignore; TRParen :: go ()
      | '+'  -> (advance src |> ignore;
        match peek src with
        | '.' -> advance src |> ignore; TFPlus :: go ()
        | ' ' -> TPlus :: go ()
        | _   -> failwith (Printf.sprintf "Unexpected character found: %c" ch))
      | '-'  -> (advance src |> ignore;
        match peek src with
        | '.' -> advance src |> ignore; TFMinus :: go ()
        | ' ' -> TMinus :: go ()
        | _   -> failwith (Printf.sprintf "Unexpected character found: %c" ch))
      | '*'  -> (advance src |> ignore;
        match peek src with
        | '.' -> advance src |> ignore; TFTimes :: go ()
        | ' ' -> TTimes :: go ()
        | _   -> failwith (Printf.sprintf "Unexpected character found: %c" ch))
      | '/'  -> (advance src |> ignore;
        match peek src with
        | '.' -> advance src |> ignore; TFSlash :: go ()
        | ' ' -> TSlash :: go ()
        | _   -> failwith (Printf.sprintf "Unexpected character found: %c" ch))
      | '<'  -> (advance src |> ignore;
        match peek src with
        | '=' -> advance src |> ignore; TLessEq :: go ()
        | _   -> failwith (Printf.sprintf "Unexpected character found: %c" ch))
      | _    ->
        if is_whitespace ch then
          begin advance src |> ignore; go () end
        else if is_digit ch then
          let n = lex_num "" in
          if String.contains n '.' then
            TFloat (float_of_string n) :: go()
          else
            TInt (int_of_string n) :: go();
        else
          failwith (Printf.sprintf "Unexpected character found: %c" ch)
    else
      []
  in
  go ()

let rec peek : token list -> token = List.hd
let rec advance : token list -> token list = List.tl

let rec consume (t:token) (toks:token list) : token list =
  match toks with
  | t' :: toks ->
    if t = t' then
      toks
    else
      failwith (Printf.sprintf "Expected '%s', found '%s'" (string_of_token t) (string_of_token t'))
  | _ -> failwith "Encountered unexpected end of token stream"

let rec parse (toks:token list) : (exp * token list) =
  if List.length toks = 0 then
    failwith "Unexpected end of token stream"
  else
    match peek toks with
    | TInt n   -> (EInt n, advance toks)
    | TTrue n  -> (EBool n, advance toks)
    | TFalse n -> (EBool n, advance toks)
    | TFloat n -> (EFloat n, advance toks)
    | TNAN     -> (ENaN nan, advance toks)
    | TLParen  -> begin
        let toks       = consume TLParen toks in
        let operator   = peek toks in
        let toks       = match peek toks with
          | TPlus    -> consume TPlus   toks;
          | TMinus   -> consume TMinus  toks;
          | TTimes   -> consume TTimes  toks;
          | TSlash   -> consume TSlash  toks;
          | TFPlus   -> consume TFPlus  toks;
          | TFMinus  -> consume TFMinus toks;
          | TFTimes  -> consume TFTimes toks;
          | TFSlash  -> consume TFSlash toks;
          | TIf      -> consume TIf     toks;
          | TLessEq  -> consume TLessEq toks;
          | t        -> failwith (Printf.sprintf "Unexpected token found: %s" (string_of_token t))
        in
        if operator = TIf then(
          let (e1, toks) = parse toks in
          let (e2, toks) = parse toks in
          let (e3, toks) = parse toks in
          let toks       = consume TRParen toks in
          match operator with
            | TIf     -> (EIf   (e1, e2, e3), toks)
            | t       -> failwith (Printf.sprintf "Unexpected token found: %s" (string_of_token t)))
        else
          let (e1, toks) = parse toks in
          let (e2, toks) = parse toks in
          let toks       = consume TRParen toks in
          match operator with
          | TPlus   -> (EAdd  (e1, e2), toks);
          | TMinus  -> (ESub  (e1, e2), toks);
          | TTimes  -> (EMuti (e1, e2), toks);
          | TSlash  -> (EFDivi (e1, e2), toks);
          | TFPlus  -> (EFAdd  (e1, e2), toks);
          | TFMinus -> (EFSub  (e1, e2), toks);
          | TFTimes -> (EFMuti (e1, e2), toks);
          | TFSlash -> (EFDivi (e1, e2), toks);
          | TLessEq -> (ELessEq (e1, e2), toks);
          | t       -> failwith (Printf.sprintf "Unexpected token found: %s" (string_of_token t))
      end
    | t      -> failwith (Printf.sprintf "Unexpected token found: %s" (string_of_token t))

let main () =
  let filename = Sys.argv.(1) in
  let tokens   = lex (Stream.of_channel (open_in filename)) in
  let (e, _)   = parse tokens in
  interpret e |> string_of_value |> print_endline

let _ = if !Sys.interactive then () else main ()
  ;;
