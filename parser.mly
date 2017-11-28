%{
open Syntax
%}

%token LPAREN RPAREN
%token LBRACE RBRACE
%token PLUS LT
%token STAR HASH

%token LIN UN
%token UNIT

/* pling & question */
%token PL QU
%token END
%token PERIOD COMMA

%token LET IN EQ
%token FUN RARROW COLON

%token FORK NEW SEND RECEIVE
%token SELECT CASE CLOSE WAIT

%token NU
%token VBAR

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
/* toplevel は exp でいいか？ */
%type <Syntax.proc> toplevel
%%

toplevel :
  | p=proc { p }

proc :
/* <e> はやめておく */
  | e=expr  { Exp e }
  | p=proc VBAR q=proc { Par(p,q) }
  | LPAREN NU c=ID COMMA d=ID RPAREN p=proc { NuBind(c,d,p) }

expr :
/*  | FUN i=ID t=ty_annot RARROW e=expr { Fun(m,i,t,e) } */
  | e1=expr e2=expr { App(e1,e2) }
/* multのつけ場所 */
/*  | LPAREN e1=expr COMMA e2=expr RPAREN { ConsPair(m,e1,e2) } */
  | LET x=ID t1=ty_annot COMMA y=ID t2=ty_annot EQ e=expr IN f=expr { DestPair(x,t1,y,t2,e,f) }
  | FORK e=expr { Fork(e) }
  | NEW { New }
  | SEND e=expr f=expr { Send(e,f) }
  | RECEIVE e=expr { Receive(e) }
  | SELECT l=ID e=expr { Select(l,e) }
/*  | CASE e=expr OF ... */
  | CLOSE e=expr { Close(e) }
  | WAIT e=expr { Wait(e) }


/* type annotation */
ty_annot :
  | COLON t=ty { t }

ty :
  | UNIT { TyUnit }
  | s=session { TySession s }
/* TODO: multの文法 */
  | t1=ty RARROW m=mult t2=ty { TyFun(m,t1,t2) }
  | t1=ty STAR m=mult t2=ty { TyProd(m,t1,t2) }
  | STAR { TyDyn }

session :
  | PL t=ty PERIOD s=session { TySend(t,s) }
  | QU t=ty PERIOD s=session { TyReceive(t,s) }
/*
  | PLUS LBRACE br=branches RBRACE { TySelect(br) }
  | AMP LBRACE br=branches RBRACE { TyCase(br) }
*/
  | END PL { TyClose }
  | END QU { TyWait }
/* 候補: LPAR STAR RPAR */
  | HASH { TyDC }

/*
branches :
*/

mult :
  | UN { Un }
  | LIN { Lin }
