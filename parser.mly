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
%token SLASH      (* / *)
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
%token COLON       (* : *)
%token TINT        (* int *)
%token TBOOL       (* bool *)
%token UNIT        (* () *)
%token TUNIT       (* unit *)
%token COMMA       (* , *)
%token FIRST       (* fst *)
%token SECOND      (* snd *)
%token LSQUARE     (* [ *)
%token RSQUARE     (* ] *)
%token EMPTYLIST   (* [] *)
%token CONS        (* :: *)
%token HEAD        (* hd *)
%token TAIL        (* tl *)
%token EMPTY       (* empty *)
%token REF         (* ref *)
%token SET         (* := *)
%token BANG        (* ! *)
%token SEMI        (* ; *)
%token WHILE       (* while *)
%token DO          (* do *)
%token END         (* end *)

%token EOF

%start <Lang.exp> prog

%left LET IN
%left SEMI
%nonassoc WHILE DO END
%left SET
%left LESSTHAN GREATERTHAN LESSEQ GREATEREQ EQUALS
%left PLUS MINUS
%left SLASH TIMES
%right CONS
%nonassoc FIRST SECOND HEAD TAIL EMPTY
%left COLON
%nonassoc BANG
%nonassoc REF
%%

prog:
  | e=exp EOF  { e }

exp:
  | e1=exp SEMI e2=exp                               { ESequence (e1, e2) }
  | WHILE e1=exp DO e2=exp END e3=exp                { ESequence ((EWhile (e1, e1, e2)), e3) }
  | e1=exp SET e2=exp                                { EAssign (e1, e2) }
  | IF e1=exp THEN e2=exp ELSE e3=exp                { EIf (e1, e2, e3) }
  | LET n=NAME COLON t=typ EQUALS e1=exp IN e2=exp   { ELet (EVar n, e1, e2, t) }
  | e1=exp PLUS e2=exp                               { (EBin (EAdd, e1, e2)) }
  | e1=exp MINUS e2=exp                              { (EBin (ESub, e1, e2)) }
  | e1=exp TIMES e2=exp                              { (EBin (EMulti, e1, e2)) }
  | e1=exp SLASH e2=exp                              { (EBin (EDivi, e1, e2)) }
  | e1=exp LESSEQ e2=exp                             { (EBin (ELessEq, e1, e2)) }
  | e1=exp GREATEREQ e2=exp                          { (EBin (EGreaterEq, e1, e2)) }
  | e1=exp LESSTHAN e2=exp                           { (EBin (EGreaterThan, e1, e2)) }
  | e1=exp GREATERTHAN e2=exp                        { (EBin (ELessThan, e1, e2)) }
  | e1=exp EQUALS e2=exp                             { (EBin (EEqual, e1, e2)) }
  | FIRST e=exp                                      { EFirst e }
  | SECOND e=exp                                     { ESecond e }
  | HEAD e=exp                                       { EHead e }
  | TAIL e=exp                                       { ETail e }
  | EMPTY e=exp                                      { EEmpty e }
  | BANG e=exp                                       { EBang e }
  | FIRST e=exp                                      { EFirst e }
  | SECOND e=exp                                     { ESecond e }
  | HEAD e=exp                                       { EHead e }
  | TAIL e=exp                                       { ETail e }
  | EMPTY e=exp                                      { EEmpty e }
  | REF e=exp                                        { ERef e }
  | f=expBase e=exp                                  { EFunctionCall (f, e) }
  | e=expBase                                        { e }

typ:
  | TINT                                             { TInt }
  | TBOOL                                            { TBool }
  | t1=typ ARROW t2=typ                              { TFun (t1, t2) }
  | TUNIT                                            { TUnit }
  | LPAREN t1=typ TIMES t2=typ RPAREN                { TPair (t1,t2) }
  | LSQUARE t=typ RSQUARE                            { TList t }
  | LESSTHAN t=typ GREATERTHAN                       { TRef t }
  | LPAREN t=typ RPAREN                              { t }

expBase:
  | FUN LPAREN n=NAME COLON t1=typ RPAREN COLON t2=typ ARROW e=exp           { EVal (VFun (EVar n, e, t1, t2)) }
  | FIX n1=NAME LPAREN n2=NAME COLON t1=typ RPAREN COLON t2=typ ARROW e=exp  { EVal (VFix (EVar n1, EVar n2, e, t1, t2)) }
  | LPAREN e1=exp COMMA e2=exp RPAREN                                        { EVal (VPair (e1, e2)) }
  | EMPTYLIST COLON t=typ                                                    { EVal (VEmptyList t) }
  | e1=exp CONS e2=exp                                                       { EVal (VCons (e1, e2)) }
  | i=INT                                                                    { EVal (VLiteral (LInt i)) }
  | b=BOOL                                                                   { EVal (VLiteral (LBool b)) }
  | n=NAME                                                                   { EVar n }
  | LPAREN e=exp RPAREN                                                      { e }
  | UNIT                                                                     { EVal VUnit }
  | LPAREN e1=exp COMMA e2=exp RPAREN                                        { EVal (VPair (e1, e2)) }
