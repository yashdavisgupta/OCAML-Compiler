%{
  open Lang
%}

%token <int> INT
%token <bool> BOOL
%token <string> NAME

%token LPAREN      (* ( *)
%token RPAREN      (* ) *)
%token PLUS        (* + *)
%token MINUS       (* - *)
%token TIMES       (* * *)
%token DIVIDE      (* / *)
%token LESSEQ      (* <= *)
%token GREATEREQ   (* >= *)
%token LESSTHAN    (* < *)
%token GREATERTHAN (* > *)
%token IF          (* if *)
%token THEN        (* then *)
%token ELSE        (* else *)
%token LET         (* let *)
%token EQUALS      (* = *)
%token IN          (* in *)
%token FUN         (* fun *)
%token ARROW       (* -> *)
%token FIX         (* fix *)

%token EOF

%start <Lang.exp> prog

%%

prog:
  | e=exp EOF  { e }

exp:
  | e1=expBase e=expBin e2=exp         { EBin (e, e1, e2) }
  | IF e1=exp THEN e2=exp ELSE e3=exp  { EIf (e1, e2, e3) }
  | LET n=NAME EQUALS e1=exp IN e2=exp { ELet (EVar n, e1, e2) }
  | f=expBase e=exp                    { EFunctionCall (f, e) }
  | e=expBase                          { e }

expBin:
  | PLUS        { EAdd }
  | MINUS       { ESub }
  | TIMES       { EMulti }
  | DIVIDE      { EDivi }
  | LESSEQ      { ELessEq }
  | GREATEREQ   { EGreaterEq }
  | LESSTHAN    { ELessThan }
  | GREATERTHAN { EGreaterThan }
  | EQUALS      { EEqual }

expBase:
  | FUN n=NAME ARROW e=exp          { EVal (VFun (EVar n, e)) }
  | FIX n1=NAME n2=NAME ARROW e=exp { EVal (VFix (EVar n1, EVar n2, e)) }
  | i=INT                           { EVal (VLiteral (LInt i)) }
  | b=BOOL                          { EVal (VLiteral (LBool b)) }
  | n=NAME                          { EVar n }
  | LPAREN e=exp RPAREN             { e }
