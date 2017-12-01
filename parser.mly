%{
open Syntax
%}

%token SEMISEMI
%token LPAREN RPAREN
%token LBRACE RBRACE

%token PLUS LT
%token STAR HASH
%token TRUE FALSE

%token LIN UN
%token UNIT

(* pling! & question? *)
%token PL QU
%token END
%token PERIOD COMMA

%token LET IN EQ
%token FUN RARROW COLON

%token FORK NEW SEND RECEIVE
%token SELECT CASE CLOSE WAIT

%token NU
%token VBAR

(* TODO:
 * 結合と、優先度 *)

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
(* toplevel は exp でいいか？ *)
%type <Syntax.proc> toplevel
%%

toplevel :
  | p=proc SEMISEMI { p }

proc :
(* <e> はやめておく *)
  | e=expr  { Exp e }
  | p=proc VBAR q=proc { Par(p,q) }
  | LPAREN NU c=ID COMMA d=ID RPAREN p=proc { NuBind(c,d,p) }

expr :
  (* | FUN i=ID t=ty_annot RARROW e=expr { Fun(m,i,t,e) }  *)
  | e1=simple_expr e2=simple_expr { App(e1,e2) }
(* multのつけ場所 *)
  (* | LPAREN e1=expr COMMA e2=expr RPAREN { ConsPair(m,e1,e2) }  *)
 (* 型注釈なし？要調査
  * 注釈が無かったら、推論しないと行けないのは、確かでは？ *)
  | LET x=ID t1=ty_annot COMMA y=ID t2=ty_annot EQ e=expr IN f=expr { DestPair(x,t1,y,t2,e,f) }
  | FORK e=expr { Fork e }
  | NEW { New }
  | SEND e=expr f=expr { Send(e,f) }
  | RECEIVE e=expr { Receive(e) }
  | SELECT l=ID e=expr { Select(l,e) }
  (* | CASE e=expr OF ...  *)
  | CLOSE e=expr { Close e }
  | WAIT e=expr { Wait e }

  | e1=expr STAR e2=expr { BinOp(Mult, e1, e2) }
  | e1=expr PLUS e2=expr { BinOp(Plus, e1, e2) }
  | e=simple_expr { e }

(* aexpr と同じ  *)
simple_expr :
  | LPAREN RPAREN { Konst KUnit }
  | v=INTV { Konst (KInt v) }
  | TRUE { Konst (KBool true) }
  | FALSE { Konst (KBool false) }
  | x=ID { Var x }
  | LPAREN e=expr RPAREN { e }


(* type annotation  *)
ty_annot :
  | COLON t=ty { t }

(* TODO:
 * 型の文法でも
 * () とか考えないといけない *)

ty :
  | UNIT { TyUnit }
  | s=session { TySession s }
 (* TODO: multの文法  *)
  | t1=ty RARROW m=mult t2=ty { TyFun(m,t1,t2) }
  | t1=ty STAR m=mult t2=ty { TyProd(m,t1,t2) }
  | STAR { TyDyn }

session :
  | PL t=ty PERIOD s=session { TySend(t,s) }
  | QU t=ty PERIOD s=session { TyReceive(t,s) }

  (* | PLUS LBRACE br=branches RBRACE { TySelect br }
   * | AMP LBRACE br=branches RBRACE { TyCase br } *)

  | END PL { TyClose }
  | END QU { TyWait }
 (* 候補: LPAR STAR RPAR  *)
  | HASH { TyDC }

(* branches : *)

mult :
  | UN { Un }
  | LIN { Lin }
