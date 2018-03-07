%{
open Lang
%}

%token <int> INT
%token <float> FLOAT

%token TRUE
%token FALSE
%token LPAREN     (* ( *)
%token RPAREN     (* ) *)
%token PLUS       (* + *)
%token MINUS      (* - *)
%token TIMES      (* * *)
%token SLASH      (* / *)
%token FPLUS      (* +. *)
%token FMINUS     (* -. *)
%token FTIMES     (* *. *)
%token FSLASH     (* /. *)
%token IF         (* if *)
%token LESSEQ     (* <= *)

%token EOF

%start <Lang.exp> prog

%%

prog:
  | e=exp EOF                           { e }

exp:
  | n= INT                              { EInt n }
  | f= FLOAT                            { EFloat f }
  | TRUE                                { EBool true  }
  | FALSE                               { EBool false  }
  | LPAREN e1=exp PLUS e2=exp RPAREN    { EAdd (e1, e2) }
  | LPAREN e1=exp MINUS e2=exp RPAREN   { ESub (e1, e2) }
  | LPAREN e1=exp TIMES e2=exp RPAREN   { EMuti (e1, e2) }
  | LPAREN e1=exp SLASH e2=exp RPAREN   { EDivi (e1, e2) }
  | LPAREN e1=exp FPLUS e2=exp RPAREN    { EFAdd (e1, e2) }
  | LPAREN e1=exp FMINUS e2=exp RPAREN   { EFSub (e1, e2) }
  | LPAREN e1=exp FTIMES e2=exp RPAREN   { EFMuti (e1, e2) }
  | LPAREN e1=exp FSLASH e2=exp RPAREN   { EFDivi (e1, e2) }
  | LPAREN IF e1=exp e2=exp e3=exp RPAREN { EIf (e1, e2, e3) }
  | LPAREN e1=exp LESSEQ e2=exp RPAREN    { ELessEq (e1, e2) }
