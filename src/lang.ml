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

let rec exp_to_string (e:exp) : string =
  match e with
  | EInt  n            -> string_of_int n
  | EBool b            -> string_of_bool b
  | EFloat f           -> string_of_float f
  | ENaN n             -> "NaN"
  | EAdd  (e1, e2)     -> "(+ "^ exp_to_string e1 ^ " " ^ exp_to_string e2 ^")"
  | ESub  (e1, e2)     -> "(- "^ exp_to_string e1 ^ " " ^ exp_to_string e2 ^")"
  | EMuti (e1, e2)     -> "(* "^ exp_to_string e1 ^ " " ^ exp_to_string e2 ^")"
  | EDivi (e1, e2)     -> "(/ "^ exp_to_string e1 ^ " " ^ exp_to_string e2 ^")"
  | EFAdd  (e1, e2)    -> "(+. "^ exp_to_string e1 ^ " " ^ exp_to_string e2 ^")"
  | EFSub  (e1, e2)    -> "(-. "^ exp_to_string e1 ^ " " ^ exp_to_string e2 ^")"
  | EFMuti (e1, e2)    -> "(*. "^ exp_to_string e1 ^ " " ^ exp_to_string e2 ^")"
  | EFDivi (e1, e2)    -> "(/. "^ exp_to_string e1 ^ " " ^ exp_to_string e2 ^")"
  | EIf   (e1, e2, e3) -> "(if "^ exp_to_string e1 ^ " " ^ exp_to_string e2 ^ " " ^ exp_to_string e3 ^ ")"
  | ELessEq (e1, e2)   -> "(<= "^ exp_to_string e1 ^ " " ^ exp_to_string e2 ^")"
