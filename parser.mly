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

%token PL QU                    (* pling!, question? *)
%token AMP                      (* ampersand& *)
%token END
%token PERIOD COMMA

%token LET IN EQ
%token FUN RARROW COLON

%token FORK NEW SEND RECEIVE
%token SELECT CLOSE WAIT
%token CASE OF SEMI

%token NU
%token VBAR

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.proc> toplevel
%%

toplevel :
  | p=proc SEMISEMI { p }

proc :
  | p=proc VBAR q=primary_proc { Par(p,q) }
    (* (nu c,d) P | Q
     * カッコを付けたい*)
  | LPAREN NU c=ID COMMA d=ID RPAREN p=proc { NuBind(c,d,p) }
  | p=primary_proc { p }

primary_proc :
  (* <e> はやめておく *)
  | e=expr { Exp e }
  | LPAREN p=proc RPAREN { p }

expr :
  (* | FUN m=mult x=ID t=ty_annot RARROW e=expr { Fun(m,x,t,e) } *)
  | FUN m=mult LPAREN x=ID t=ty_annot RPAREN RARROW e=expr
    { Fun(m,x,t,e) }
 (* 型注釈なし？要調査
  * 注釈が無かったら、推論しないと行けないのは、確かでは？ *)
  | LET x=ID COMMA y=ID EQ e=plus_expr IN f=expr
    { PairDest(x,y,e,f) }

  (* let (x:t) = e in f
   * is equivalent to
   * (fun lin (x:t) -> f) e *)
  (* | LET LPAREN x=ID t=ty_annot RPAREN EQ e=plus_expr
   *   IN f=expr { App (Fun(Lin,x,t,f), e) } *)
  (* TODO: let 式は primitive として追加しないといけない
   * 追加すれば e を先に型付けすることで、tが分かる (注釈不要) *)

  | CASE e=plus_expr OF
    LBRACE br=separated_list(SEMI, branch) RBRACE
    { Case (e,br) }

  | e=plus_expr { e }

(* case branch *)
branch:
  (* | l=ID COLON x=ID PERIOD e=expr { (l,x,e) } *)
  (* | l=ID COLON LPAREN x=ID t=ty_annot RPAREN PERIOD e=expr *)
  | l=ID COLON x=ID PERIOD e=expr { (l,x,e) }

plus_expr :
  | e1=plus_expr PLUS e2=mult_expr { BinOp(Plus, e1, e2) }
  | e=mult_expr { e }

mult_expr :
  | e1=mult_expr STAR e2=app_expr { BinOp(Mult, e1, e2) }
  | e=app_expr { e }

app_expr :
  | e1=app_expr e2=primary_expr { App(e1,e2) }
  | FORK e=primary_expr { Fork e }
  | NEW LPAREN s=session RPAREN { New s } (* 結合順位これで良い?? *)
  | SEND e=primary_expr f=primary_expr { Send(e,f) }
  | RECEIVE e=primary_expr { Receive e }
  | SELECT l=ID e=primary_expr { Select(l,e) }
  | CLOSE e=primary_expr { Close e }
  | WAIT e=primary_expr { Wait e }
  | e=primary_expr { e }


(* aexpr と同じ  *)
primary_expr :
  | LPAREN RPAREN { UnitV }
  | v=INTV { IntV v }
  | TRUE { BoolV true }
  | FALSE { BoolV false }
  | x=ID { Var x }
  (* multのつけ場所 ?? *)
  | LPAREN e1=plus_expr COMMA e2=plus_expr RPAREN m=mult { PairCons(m,e1,e2) }
  | LPAREN e=expr RPAREN { e }


(* type annotation  *)
ty_annot :
  | COLON t=ty { t }

ty :
 (* TODO: multの文法  *)
  | t1=primary_ty RARROW m=mult t2=ty { TyFun(m,t1,t2) }
  | t1=primary_ty STAR m=mult t2=ty { TyProd(m,t1,t2) }
  | s=session { TySession s }
  | t=primary_ty { t }

primary_ty :
  | UNIT { TyUnit }
  | INT { TyInt }
  | BOOL { TyBool }
  | STAR { TyDyn }
  | LPAREN t=ty RPAREN { t }

session :
  | PL t=primary_ty PERIOD s=session { TySend(t,s) }
  | QU t=primary_ty PERIOD s=session { TyReceive(t,s) }

  | PLUS LBRACE br=separated_list(COMMA, branch_ty) RBRACE { TySelect br }
  | AMP LBRACE br=separated_list(COMMA, branch_ty) RBRACE { TyCase br }
  | s=primary_session { s }

branch_ty :
  | l=ID COLON s=session { (l,s) }

primary_session :
  | END PL { TyClose }
  | END QU { TyWait }
  | HASH { TyDC }
  | LPAREN s=session RPAREN { s }

mult :
  | UN { Un }
  | LIN { Lin }
