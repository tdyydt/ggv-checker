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
%token UNIT INT BOOL

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
(* associativity & precedence *)
(* lowest to highest *)
(* %left PLUS
 * %left STAR *)
(* STARって2通りの意味がある。 *)

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
(* toplevel は exp でいいか？ *)
%type <Syntax.proc> toplevel
%%

toplevel :
  | p=proc SEMISEMI { p }

proc :
  | p=primary_proc VBAR q=primary_proc { Par(p,q) }
    (* (nu c,d) P | Q
     * カッコを付けたい*)
  | LPAREN NU c=ID COMMA d=ID RPAREN p=proc { NuBind(c,d,p) }
  | p=primary_proc { p }

primary_proc :
  (* <e> はやめておく *)
  | e=expr { Exp e }
  | LPAREN p=proc RPAREN { p }

expr :
  | FUN m=mult i=ID t=ty_annot RARROW e=expr { Fun(m,i,t,e) }
  | e1=primary_expr e2=primary_expr { App(e1,e2) }
(* multのつけ場所 *)
  | LPAREN e1=expr COMMA e2=expr RPAREN m=mult { ConsPair(m,e1,e2) }
 (* 型注釈なし？要調査
  * 注釈が無かったら、推論しないと行けないのは、確かでは？ *)
  | LET x=ID t1=ty_annot COMMA y=ID t2=ty_annot EQ e=expr IN f=expr { DestPair(x,t1,y,t2,e,f) }
  | FORK e=primary_expr { Fork e }
  | NEW { New }
  | SEND e=primary_expr f=primary_expr { Send(e,f) }
  | RECEIVE e=primary_expr { Receive e }
  | SELECT l=ID e=primary_expr { Select(l,e) }
  (* | CASE e=expr OF ...  *)
  | CLOSE e=primary_expr { Close e }
  | WAIT e=primary_expr { Wait e }

  | e1=expr STAR e2=expr { BinOp(Mult, e1, e2) }
  | e1=expr PLUS e2=expr { BinOp(Plus, e1, e2) }
  | e=primary_expr { e }

(* aexpr と同じ  *)
primary_expr :
  | LPAREN RPAREN { Konst KUnit }
  | v=INTV { Konst (KInt v) }
  | TRUE { Konst (KBool true) }
  | FALSE { Konst (KBool false) }
  | x=ID { Var x }
  | LPAREN e=expr RPAREN { e }


(* type annotation  *)
ty_annot :
  | COLON t=ty { t }

ty :
 (* TODO: multの文法  *)
  | t1=ty RARROW m=mult t2=ty { TyFun(m,t1,t2) }
  | t1=ty STAR m=mult t2=ty { TyProd(m,t1,t2) }
  | t=primary_ty { t }

primary_ty :
  | UNIT { TyUnit }
  | INT { TyInt }
  | BOOL { TyBool }
  | s=session { TySession s }
  | STAR { TyDyn }
  | LPAREN t=ty RPAREN { t }

session :
  | PL t=ty PERIOD s=session { TySend(t,s) }
  | QU t=ty PERIOD s=session { TyReceive(t,s) }

  (* | PLUS LBRACE br=branches RBRACE { TySelect br }
   * | AMP LBRACE br=branches RBRACE { TyCase br } *)
  | s=primary_session { s }

primary_session :
  | END PL { TyClose }
  | END QU { TyWait }
  | HASH { TyDC }
  | LPAREN s=session RPAREN { s }

(* branches :
 *   | l=ID COLON s=session { (l,s) }
 *   | separated_list (COMMA, )
 *
 * branch :
 *       | *)

mult :
  | UN { Un }
  | LIN { Lin }
