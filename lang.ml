type literal =
  | LInt of int
  | LBool of bool

type expBin =
  | EAdd
  | ESub
  | EMulti
  | EDivi
  | ELessEq
  | EGreaterEq
  | ELessThan
  | EGreaterThan
  | EEqual

type value =
  | VLiteral of literal
  | VFun of exp * exp
  | VFix of exp * exp * exp
and exp =
  | EVal of value
  | EBin of expBin * exp * exp
  | EIf of exp * exp * exp
  | EVar of string
  | ELet of exp * exp * exp
  | EFunctionCall of exp * exp

let exp_to_stringBin (e: expBin) : string =
  match e with
  | EAdd          -> "+"
  | ESub          -> "-"
  | EMulti        -> "*"
  | EDivi         -> "/"
  | ELessEq       -> "<="
  | EGreaterEq    -> ">="
  | ELessThan     -> "<"
  | EGreaterThan  -> ">"
  | EEqual        -> "="

let exp_to_stringBin (e: expBin) =
  match e with
  | EAdd          -> "+"
  | ESub          -> "-"
  | EMulti        -> "*"
  | EDivi         -> "/"
  | ELessThan     -> "<"
  | ELessEq       -> "<="
  | EGreaterThan  -> ">"
  | EGreaterEq    -> ">="
  | EEqual        -> "="

let rec exp_to_string (e: exp) : string =
  match e with
  | EVal v -> value_to_string v
  | EBin (e, e1, e2)  -> "(" ^ (exp_to_stringBin e) ^ " " ^ (exp_to_string e1) ^ " " ^ (exp_to_string e2) ^ ")"
  | EIf (e1, e2, e3) -> "(if " ^ (exp_to_string e1) ^ " " ^ (exp_to_string e2) ^ " " ^ (exp_to_string e3) ^ ")"
  | EVar s -> s
  | ELet (e1, e2, e3) -> ("(let " ^ (exp_to_string e1) ^ " = " ^ (exp_to_string e2) ^ " in " ^ (exp_to_string e3) ^ ")")
  | EFunctionCall (e1, e2) -> ("(" ^ (exp_to_string e1) ^ " " ^ (exp_to_string e2) ^ ")")
and value_to_string (v: value) : string =
  match v with
  | VLiteral (LInt i) -> string_of_int i
  | VLiteral (LBool b) -> string_of_bool b
  | VFun (e1, e2) -> ("(fun " ^ (exp_to_string e1) ^ " " ^ (exp_to_string e2) ^ ")")
  | VFix (e1, e2, e3) -> ("(fix " ^ (exp_to_string e1) ^ " " ^ (exp_to_string e2) ^ " " ^ (exp_to_string e3) ^ ")")

let rec exp_to_string' (e: exp) : string =
  match e with
  | EVal v -> value_to_string' v
  | EBin (e, e1, e2) -> ("(" ^ (exp_to_string' e1) ^ " " ^ (exp_to_stringBin e) ^ " " ^ (exp_to_string' e2) ^ ")")
  | EIf (e1, e2, e3) -> ("(if " ^ (exp_to_string' e1) ^ " then " ^ (exp_to_string' e2) ^ " else " ^ (exp_to_string' e3) ^ ")")
  | EVar s -> s
  | ELet (e1, e2, e3) -> ("(let " ^ (exp_to_string' e1) ^ " = " ^ (exp_to_string' e2) ^ " in " ^ (exp_to_string' e3) ^ ")")
  | EFunctionCall (e1, e2) -> ("(" ^ (exp_to_string' e1) ^ " " ^ (exp_to_string' e2) ^ ")")
and value_to_string' (v: value) : string =
  match v with
  | VLiteral (LInt i) -> string_of_int i
  | VLiteral (LBool b) -> string_of_bool b
  | VFun (e1, e2) -> ("(fun " ^ (exp_to_string' e1) ^ " -> " ^ (exp_to_string' e2) ^ ")")
  | VFix (e1, e2, e3) -> ("(fix " ^ (exp_to_string' e1) ^ " " ^ (exp_to_string' e2) ^ " -> " ^ (exp_to_string' e3) ^ ")")

let rec interpret' (v: value) (s: string) (e: exp) : exp =
  match e with
  | EVal v' ->
    begin
      match v' with
      | VLiteral l -> EVal (VLiteral l)
      | VFun (EVar str, e') -> (if (compare str s) = 0
        then e
        else EVal (VFun (EVar str, (interpret' v s e'))))
      | VFix (EVar f, EVar x, e') ->
        begin
        match (compare f s), (compare x s) with
        | 0,_ -> e
        | _,0 -> e
        | _ -> EVal (VFix (EVar f, EVar x, (interpret' v s e')))
        end
      | _ -> failwith "Interpret': unknown value"
    end
  | EBin (e, e1, e2) -> EBin (e, (interpret' v s e1 ), (interpret' v s e2))
  | EIf (e1, e2, e3) -> EIf ((interpret' v s e1), (interpret' v s e2), (interpret' v s e3))
  | EVar str -> (if (compare str s) = 0 then EVal v else e)
  | ELet (EVar str, e1, e2) -> (if (compare str s) = 0 then e
    else ELet (EVar str, (interpret' v s e1), (interpret' v s e2)))
  | EFunctionCall (f, e2) -> EFunctionCall ((interpret' v s f), (interpret' v s e2))
  | _ -> failwith "Interpret': unknown expression"
let val_to_int v =
  match v with
  | VLiteral (LInt i) -> i
  | _ -> failwith "Error, val_to_int needs a val of int"

let val_to_bool v =
  match v with
  | VLiteral (LBool b) -> b
  | _ -> failwith "Error, val_to_bool needs a val of bool"

let oldInterpret (e: expBin) (v1: value) (v2: value) : value =
  let i1 = val_to_int v1 in
  let i2 = val_to_int v2 in
  match e with
  | EAdd           -> VLiteral (LInt (i1 + i2))
  | ESub           -> VLiteral (LInt (i1 - i2))
  | EMulti         -> VLiteral (LInt (i1 * i2))
  | EDivi          -> VLiteral (LInt (i1 / i2))
  | ELessEq        -> VLiteral (LBool (i1 <= i2))
  | EGreaterEq     -> VLiteral (LBool (i1 >= i2))
  | ELessThan      -> VLiteral (LBool (i1 < i2))
  | EGreaterThan   -> VLiteral (LBool (i1 > i2))
  | EEqual         -> VLiteral (LBool (i1 = i2))

let is_value (e: exp) : bool =
  match e with
  | EVal _ -> true
  | _ -> false

let exp_to_value (e: exp) : value =
  match e with
  | EVal v -> v
  | _ -> failwith ("exp_to_value called with non-value expression: " ^ (exp_to_string' e))

let rec interpret (e: exp) : exp =
  match e with
  | EVal v -> e
  | EBin (e, e1, e2) ->
    begin
      let e1' = not (is_value e1) in
      let e2' = not (is_value e2) in
      match e1', e2' with
      | true, _ -> EBin (e, interpret e1, e2)
      | false, true -> EBin (e, e1, interpret e2)
      | false, false -> EVal (oldInterpret e (exp_to_value e1) (exp_to_value e2))
    end
  | EIf (e1, e2, e3) ->
    begin
      let e1' = not (is_value e1) in
      match e1' with
      | true -> EIf (interpret e1, e2, e3)
      | false -> if (val_to_bool (exp_to_value e1)) then (interpret e2) else (interpret e3)
    end
  | ELet (EVar s, e1, e2) ->
    begin
      let e1' = not (is_value e1) in
      if e1' then ELet (EVar s, interpret e1, e2)
      else interpret' (exp_to_value e1) s e2
    end
  | EVar s -> failwith (s ^ " isn't a variable");
  | EFunctionCall (e1, e2) ->
    begin
    let e2' = not (is_value e2) in
    if e2' then EFunctionCall (e1, interpret e2)
    else
      match e1 with
      | EFunctionCall _ -> EFunctionCall (interpret e1, e2)
      | EVal (VFun (EVar s, f)) -> interpret' (exp_to_value e2) s f
      | EVal (VFix (EVar f, EVar x, e)) ->
        let interpret'_x = interpret' (exp_to_value e2) x e in
        interpret' (exp_to_value e1) f interpret'_x
      | _ -> failwith ("Check your formatting")
    end
  | _ -> failwith "That isn't an expression"

let eval (e: exp) : value =
  let rec eval' (e: exp) : exp =
  if is_value e then
    e
  else eval'(interpret e) in
  exp_to_value (eval' e)

let eval_ret (e: exp) : unit =
  let rec eval' (e: exp) : exp =
    if is_value e then
      e
    else
      (let result = interpret e in
      print_endline ("→   " ^ (exp_to_string' result));
      eval'(result)) in
  print_endline ("    " ^ exp_to_string' e);
  eval' e; ();
