%{
open Lang
%}

%token <int> INT
%token <float> FLOAT

%token TRUE
%token FALSE
%token NULL
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
  | LPAREN PLUS e1=exp e2=exp RPAREN    { EAdd (e1, e2) }parser.mll
  | LPAREN MINUS e1=exp e2=exp RPAREN   { ESub (e1, e2) }parser.mll
  | LPAREN TIMES e1=exp e2=exp RPAREN   { EMulti (e1, e2) }parser.mll
  | LPAREN SLASH e1=exp e2=exp RPAREN   { EDivi (e1, e2) }parser.mll
