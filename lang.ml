open List

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

type typ =
  | TInt
  | TBool
  | TFun of typ * typ
  | TUnit
  | TPair of typ * typ
  | TList of typ

type value =
  | VLiteral of literal
  | VFun of exp * exp * typ * typ
  | VFix of exp * exp * exp * typ * typ
  | VUnit
  | VPair of exp * exp
  | VEmptyList of typ
  | VCons of exp * exp
and exp =
  | EVal of value
  | EBin of expBin * exp * exp
  | EIf of exp * exp * exp
  | EVar of string
  | ELet of exp * exp * exp * typ
  | EFunctionCall of exp * exp
  | EFirst of exp
  | ESecond of exp
  | EHead of exp
  | ETail of exp
  | EEmpty of exp

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

let rec string_of_type (t: typ) : string =
  match t with
  | TInt -> "int"
  | TBool -> "bool"
  | TFun (t1, t2) -> ((string_of_type t1) ^ " -> " ^ (string_of_type t2))
  | TUnit -> "unit"
  | TPair (t1, t2) -> ("(" ^ (string_of_type t1) ^ " * " ^ (string_of_type t2) ^ ")")
  | TList t -> ("[" ^ (string_of_type t) ^ "]")

let rec exp_to_string (e: exp) : string =
  match e with
  | EVal v -> value_to_string v
  | EBin (e, e1, e2)  -> "(" ^ (exp_to_stringBin e) ^ " " ^ (exp_to_string e1) ^ " " ^ (exp_to_string e2) ^ ")"
  | EIf (e1, e2, e3) -> "(if " ^ (exp_to_string e1) ^ " " ^ (exp_to_string e2) ^ " " ^ (exp_to_string e3) ^ ")"
  | EVar s -> s
  | ELet (e1, e2, e3, t) -> ("(let " ^ (exp_to_string e1) ^ " : " ^ (string_of_type t) ^ " = " ^ (exp_to_string e2) ^ " in " ^ (exp_to_string e3) ^ ")")
  | EFunctionCall (e1, e2) -> ("(" ^ (exp_to_string e1) ^ " " ^ (exp_to_string e2) ^ ")")
  | EFirst expr -> ("(fst " ^ (exp_to_string expr) ^ ")")
  | ESecond expr -> ("(snd " ^ (exp_to_string expr) ^ ")")
  | EHead expr -> ("(head " ^ (exp_to_string expr) ^ ")")
  | ETail expr -> ("(tail " ^ (exp_to_string expr) ^ ")")
  | EEmpty expr -> ("(empty " ^ (exp_to_string expr) ^ ")")
and value_to_string (v: value) : string =
  match v with
  | VLiteral (LInt i) -> string_of_int i
  | VLiteral (LBool b) -> string_of_bool b
  | VFun (e1, e2, t1, t2) -> ("(fun (" ^ (exp_to_string e1) ^ ": " ^ (string_of_type t1) ^ ") : " ^ (string_of_type t2) ^ " -> " ^ (exp_to_string e2) ^ ")")
  | VFix (e1, e2, e3, t1, t2) -> ("(fix " ^ (exp_to_string e1) ^ " (" ^ (exp_to_string e2) ^ ": " ^ (string_of_type t1) ^ ") : " ^ (string_of_type t2) ^ " -> " ^ (exp_to_string e3) ^ ")")
  | VUnit -> "()"
  | VPair (e1, e2) -> ("(" ^ (exp_to_string e1) ^ ", " ^ (exp_to_string e2) ^ ")")
  | VEmptyList t -> ("([] : " ^ (string_of_type t) ^ ")")
  | VCons (e1, e2) -> ("(" ^ (exp_to_string e1) ^ " :: " ^ (exp_to_string e2) ^ ")")

let rec exp_to_string' (e: exp) : string =
  match e with
  | EVal v -> value_to_string' v
  | EBin (e, e1, e2) -> ("(" ^ (exp_to_string' e1) ^ " " ^ (exp_to_stringBin e) ^ " " ^ (exp_to_string' e2) ^ ")")
  | EIf (e1, e2, e3) -> ("(if " ^ (exp_to_string' e1) ^ " then " ^ (exp_to_string' e2) ^ " else " ^ (exp_to_string' e3) ^ ")")
  | EVar s -> s
  | ELet (e1, e2, e3, t) -> ("(let " ^ (exp_to_string' e1) ^ " : " ^ (string_of_type t) ^ " = " ^ (exp_to_string' e2) ^ " in " ^ (exp_to_string' e3) ^ ")")
  | EFunctionCall (e1, e2) -> ("(" ^ (exp_to_string' e1) ^ " " ^ (exp_to_string' e2) ^ ")")
  | EFirst expr -> ("(fst " ^ (exp_to_string' expr) ^ ")")
  | ESecond expr -> ("(snd " ^ (exp_to_string' expr) ^ ")")
  | EHead expr -> ("(head " ^ (exp_to_string' expr) ^ ")")
  | ETail expr -> ("(tail " ^ (exp_to_string' expr) ^ ")")
  | EEmpty expr -> ("(empty " ^ (exp_to_string' expr) ^ ")")
and value_to_string' (v: value) : string =
  match v with
  | VLiteral (LInt i) -> string_of_int i
  | VLiteral (LBool b) -> string_of_bool b
  | VFun (e1, e2, t1, t2) -> ("(fun (" ^ (exp_to_string' e1) ^ ":" ^ (string_of_type t1) ^ ") : " ^ (string_of_type t2) ^ " -> " ^ (exp_to_string' e2) ^ ")")
  | VFix (e1, e2, e3, t1, t2) -> ("(fix " ^ (exp_to_string' e1) ^ " (" ^ (exp_to_string' e2) ^ ":" ^ (string_of_type t1) ^ ") : (" ^ (string_of_type t2) ^ ") -> " ^ (exp_to_string' e3) ^ ")")
  | VUnit -> "()"
  | VPair (e1, e2) -> ("(" ^ (exp_to_string' e1) ^ ", " ^ (exp_to_string' e2) ^ ")")
  | VEmptyList t -> ("[] : " ^ (string_of_type t))
  | VCons (e1, e2) -> ((exp_to_string' e1) ^ " :: " ^ (exp_to_string' e2))

let rec compare_type (t1: typ) (t2: typ) : bool =
  match t1, t2 with
  | TInt, TInt -> true
  | TBool, TBool -> true
  | TFun (t1, t2), TFun (t3, t4) -> (compare_type t1 t3) && (compare_type t2 t4)
  | TUnit, TUnit -> true
  | TPair (t1, t2), TPair (t3, t4) -> (compare_type t1 t3) && (compare_type t2 t4)
  | TList t1, TList t2 -> (compare_type t1 t2)
  | _ -> false

let expBin_to_type_in (e: expBin) : typ =
  match e with
  | _ -> TInt

let expBin_to_type_out (e: expBin) : typ =
  match e with
  | EAdd | ESub
  | EMulti | EDivi -> TInt
  | _ -> TBool

let rec typecheck (c: (string * typ) list) (e: exp) : typ =
  match e with
  | EVal (VLiteral (LInt _)) -> TInt
  | EVal (VLiteral (LBool _)) -> TBool
  | EVal (VFun (EVar s, e', t1, t2)) ->
    let e_type = typecheck (cons (s, t1) c) e' in
    if compare_type e_type t2 then TFun (t1, t2)
    else failwith ("Function typechecking failed, expected return type of " ^ (string_of_type (TFun (t1, t2))) ^ ", instead recieved " ^ (string_of_type (TFun (t1, e_type))))
  | EVal (VFix (EVar f, EVar s, e, t1, t2)) ->
    let e_type = typecheck (cons (f, TFun(t1,t2)) (cons (s, t1) c)) e in
    if compare_type e_type t2 then TFun (t1, t2)
    else failwith ("Fixpoint typechecking failed, expected return type of " ^ (string_of_type (TFun (t1, t2))) ^ ", instead recieved " ^ (string_of_type (TFun (t1, e_type))))
  | EVal (VUnit) -> TUnit
  | EVal (VPair (e1, e2)) -> TPair ((typecheck c e1), (typecheck c e2))
  | EVal (VEmptyList t) -> TList t
  | EVal (VCons (e1, e2)) ->
    begin
      let e1_type = (typecheck c e1) in
      let e2_type = (typecheck c e2) in
      match e1_type, e2_type with
      | t1, TList t2 -> (if (compare_type t1 t2) then TList t1 else failwith ("Cons typechecking failed, e1 should have the same type as e2"))
      | _ -> failwith ("Cons typechecking failed, e2 should have type list, instead recieved " ^ (string_of_type e2_type))
    end
  | EBin (e, e1, e2) ->
    let in_type = expBin_to_type_in e in
    let e1_type = typecheck c e1 in
    let e2_type = typecheck c e2 in
    if (compare_type e1_type in_type) && (compare_type e2_type in_type) then expBin_to_type_out e
    else failwith ("Binary Expression typechecking failed, expected input type: " ^ (string_of_type in_type) ^ ", instead recieved " ^ (string_of_type e1_type) ^ " and " ^ (string_of_type e2_type))
  | EIf (e1, e2, e3) ->
    let e1_type = typecheck c e1 in
    let e2_type = typecheck c e2 in
    let e3_type = typecheck c e3 in
    if (compare_type e1_type TBool) && (compare_type e2_type e3_type)
    then e2_type
    else failwith
        ("If typechecking failed, expected format: if <bool> then <t> else <t>, instead recieved " ^ (string_of_type e1_type) ^ ", " ^ (string_of_type e2_type) ^ ", " ^ (string_of_type e3_type))
  | EVar s -> List.assoc s c
  | ELet (EVar s, e1, e2, t) ->
    let e1_type = typecheck c e1 in
    let e2_type = typecheck (cons (s, t) c) e2 in
    if (compare_type t e1_type) then e2_type
    else failwith ("Let typechecking failed, expected binding type: " ^ (string_of_type t) ^ ", instead recieved " ^ (string_of_type e1_type))
  | EFunctionCall (e1, e2) ->
    begin
      let e1_type = typecheck c e1 in
      let e2_type = typecheck c e2 in
      match e1_type with
      | TFun (t1, t2) ->
        begin
          if compare_type t1 e2_type then t2
          else failwith ("Fun call typechecking failed, expected input type: " ^ (string_of_type t1) ^ ", instead recieved " ^ (string_of_type e2_type) ^ (exp_to_string' e1) ^ " " ^ (exp_to_string' e2))
        end
      | _ -> failwith ("Fun call typechecking error, first expression should be of type function, instead recieved " ^ (string_of_type e1_type))
    end
  | EFirst ex ->
    begin
      let e_type = (typecheck c ex) in
      match e_type with
      | TPair (t1, t2) -> t1
      | _ -> failwith ("Typechecking failed, exp should be of type pair, instead recieved " ^ (string_of_type e_type))
    end
  | ESecond ex ->
    begin
      let e_type = (typecheck c ex) in
      match e_type with
      | TPair (t1, t2) -> t2
      | _ -> failwith ("Typechecking failed, exp should be of type pair, instead recieved " ^ (string_of_type e_type))
    end
  | EHead ex ->
    begin
      let e_type = (typecheck c ex) in
      match e_type with
      | TList t -> t
      | _ -> failwith ("Typechecking failed, exp should be of type list, instead recieved " ^ (string_of_type e_type))
    end
  | ETail ex ->
    begin
      let e_type = (typecheck c ex) in
      match e_type with
      | TList t -> TList t
      | _ -> failwith ("Tail typechecking failed, exp should be of type list, , instead recieved " ^ (string_of_type e_type))
    end
  | EEmpty ex ->
    begin
      let e_type = (typecheck c ex) in
      match e_type with
      | TList t -> TBool
      | _ -> failwith ("empty typechecking failed, exp should be of type list, instead recieved " ^ (string_of_type e_type))
    end
  | _ -> failwith ("Typechecking failed, some formatting issue in string: " ^ (exp_to_string' e))

let rec interpret' (v: value) (s: string) (e: exp) : exp =
  match e with
  | EVal v' ->
    begin
      match v' with
      | VLiteral l -> e
      | VFun (EVar str, e', t1, t2) -> (if (compare str s) = 0
                                        then e
                                        else EVal (VFun (EVar str, (interpret' v s e'), t1, t2)))
      | VFix (EVar f, EVar x, e', t1, t2) ->
        begin
          match (compare f s), (compare x s) with
          | 0,_ -> e
          | _,0 -> e
          | _ -> EVal (VFix (EVar f, EVar x, (interpret' v s e'), t1, t2))
        end
      | VUnit -> e
      | VPair (e1, e2) -> EVal (VPair ((interpret' v s e1), (interpret' v s e2)))
      | VEmptyList t -> EVal (VEmptyList t)
      | VCons (e1, e2) -> EVal (VCons ((interpret' v s e1), (interpret' v s e2)))
      | _ -> failwith "Interpret': unknown value"
    end
  | EBin (e, e1, e2) -> EBin (e, (interpret' v s e1 ), (interpret' v s e2))
  | EIf (e1, e2, e3) -> EIf ((interpret' v s e1), (interpret' v s e2), (interpret' v s e3))
  | EVar str -> (if (compare str s) = 0 then EVal v else e)
  | ELet (EVar str, e1, e2, t) -> (if (compare str s) = 0 then e
                                   else ELet (EVar str, (interpret' v s e1), (interpret' v s e2), t))
  | EFunctionCall (f, e2) -> EFunctionCall ((interpret' v s f), (interpret' v s e2))
  | EFirst ex -> EFirst (interpret' v s ex)
  | ESecond ex -> ESecond (interpret' v s ex)
  | EHead ex -> EHead (interpret' v s ex)
  | ETail ex -> ETail (interpret' v s ex)
  | EEmpty ex -> EEmpty (interpret' v s ex)
  | _ -> failwith "Interpret': unknown expression"
let val_to_int v =
  match v with
  | VLiteral (LInt i) -> i
  | _ -> failwith "Error, val_to_int needs a val of type int"

let val_to_bool v =
  match v with
  | VLiteral (LBool b) -> b
  | _ -> failwith "Error, val_to_bool needs a val of type bool"

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
  | _ -> failwith ("exp_to_value called with incompatable expression: " ^ (exp_to_string' e))

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
  | ELet (EVar s, e1, e2, t) ->
    begin
      let e1' = not (is_value e1) in
      if e1' then ELet (EVar s, interpret e1, e2, t)
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
        | EVal (VFun (EVar s, f, t1, t2)) -> interpret' (exp_to_value e2) s f
        | EVal (VFix (EVar f, EVar x, e, t1, t2)) ->
          let interpret'_x = interpret' (exp_to_value e2) x e in
          interpret' (exp_to_value e1) f interpret'_x
        | _ -> failwith ("Check your formatting")
    end
  | EFirst (EVal (VPair (e1, e2))) -> e1
  | ESecond (EVal (VPair (e1, e2))) -> e2
  | EHead ex ->
    begin
      match ex with
      | EVal (VEmptyList _) -> failwith ("Head: Empty List")
      | EVal (VCons (e1, e2)) -> e1
      | _ -> failwith("Head Typechecking Error")
    end
  | ETail ex ->
    begin
      match ex with
      | EVal (VEmptyList _) -> failwith ("Tail: Empty list")
      | EVal (VCons (e1, e2)) -> e2
      | _ -> failwith("Head Typechecking Error")
    end
  | EEmpty ex ->
    begin
      match ex with
      | EVal (VEmptyList _) -> EVal (VLiteral (LBool true))
      | _ -> EVal (VLiteral (LBool false))
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
    if is_value e then e
    else
      (let result = interpret e in
       print_endline ("→   " ^ (exp_to_string' result));
       eval'(result)) in
  print_endline ("    " ^ exp_to_string' e);
  eval' e; ();
