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
  | e=expr { Exp e }

expr :
  | FUN i=ID ty_annot RARROW e=expr { Fun(i,e) }
  |


/* type annotation */
ty_annot :
  | COLON t=ty { t }

ty :
  | UNIT
  | session
/* TODO: multの文法 */
  | t=ty RARROW m=mult u=ty { TyFun(m,t,u) }
  | t=ty STAR m=mult u=ty { TyProd(m,t,u) }

session :
  | PL t=ty PERIOD s=session { TySend(t,s) }
  | QU t=ty PERIOD s=session { TyReceive(t,s) }
  | PLUS LBRACE br=branches RBRACE { TySelect(br) }
  | AMP LBRACE br=branches RBRACE { TyCase(br) }

mult :
  | UN { Un }
  | LIN { Lin }
