%{
open Syntax
%}

%token LPAREN RPAREN
%token LBRACE RBRACE
%token PLUS MULT LT

%token LIN UN
%token UNIT

%token LET IN EQ
%token FUN RARROW COLON

%token FORK NEW SEND REVEIVE
%token SELECT CASE CLOSE WAIT

/* pling & question */
%token PL QU
%token END

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
/* toplevel は exp でいいか？ */
%type <Syntax.proc> toplevel
%%

toplevel :
  | p=proc SEMISEMI { p }

proc :
  | e=expr  { Exp e }
  | p=proc VBAR q=proc { Par(p,q) }
  | LPAR NU c=ID COMMA d=ID RPAR p=proc { NuBind(c,d,p) }

expr :
  | FUN i=ID ty_annot RARROW e=expr { Fun(i,e) }
  | e1=expr e2=expr { App(e1,e2) }
/* multのつけ場所 */
  | LPAR e1=expr COMMA e2=expr RPAR { ConsPair(m,e1,e2) }
  | LET x=ID t1=ty_annot COMMA y=ID t2=ty_annot EQ e=exp IN f=exp { DestPair(x,t1,y,t2,e,f) }
  | FORK e=exp { Fork(e) }
  | NEW { New }
  | SEND e=exp f=exp { Send(e,f) }
  | RECEIVE e=exp { Receive(e) }
  | SELECT l=ID e=exp { Select(l,e) }
  | CASE e=exp OF ...
  | CLOSE e=exp { Close(e) }
  | WAIT e=exp { Wait(e) }


/* type annotation */
ty_annot :
  | COLON t=ty { t }

ty :
  | UNIT
  | session
/* TODO: multの文法 */
  | t1=ty RARROW m=mult t2=ty { TyFun(m,t1,t2) }
  | t1=ty STAR m=mult t2=ty { TyProd(m,t1,t2) }
  | STAR { TyDyn }

session :
  | PL t=ty PERIOD s=session { TySend(t,s) }
  | QU t=ty PERIOD s=session { TyReceive(t,s) }
  | PLUS LBRACE br=branches RBRACE { TySelect(br) }
  | AMP LBRACE br=branches RBRACE { TyCase(br) }
  | END PL { TyClose }
  | END QU { TyWait }
/* 候補: LPAR STAR RPAR */
  | HASH { TyDC }

branches :

mult :
  | UN { Un }
  | LIN { Lin }
