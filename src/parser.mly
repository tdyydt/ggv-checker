%{
open Syntax
%}

%token SEMISEMI
%token LPAREN RPAREN
%token LBRACE RBRACE

%token LIN UN
%token UNIT INT BOOL
%token BANG QU AMP
%token END PERIOD COMMA

%token PLUS MINUS STAR HASH
(* %token LT *)
%token TRUE FALSE
%token LET IN EQ
%token FUN RARROW COLON
%token FORK NEW SEND RECEIVE
%token SELECT CLOSE WAIT
%token CASE OF SEMI

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.exp> toplevel
%%

toplevel :
  | e=expr SEMISEMI { e }

expr :
  (* parentheses needed ?? *)
  (* fun (x:T) -> e *)
  | FUN m=mult LPAREN x=ID t=ty_annot RPAREN RARROW e=expr
    { FunExp(m,x,t,e) }

  | LET x=ID COMMA y=ID EQ e=plus_expr IN f=expr
    { PairDest(x,y,e,f) }

  (* let expression *)
  | LET x=ID EQ e=plus_expr IN f=expr { LetExp (x,e,f) }

  | CASE e=plus_expr OF
    LBRACE br=separated_list(SEMI, branch) RBRACE
    { CaseExp (e,br) }

  | e=plus_expr { e }

(* case branch *)
branch:
  (* | l=ID COLON x=ID PERIOD e=expr { (l,x,e) } *)
  (* | l=ID COLON LPAREN x=ID t=ty_annot RPAREN PERIOD e=expr *)
  | l=ID COLON x=ID PERIOD e=expr { (l,x,e) }

plus_expr :
  | e1=plus_expr PLUS e2=mult_expr { BinOp(Plus, e1, e2) }
  | e1=plus_expr MINUS e2=mult_expr { BinOp(Minus, e1, e2) }
  | e=mult_expr { e }

mult_expr :
  | e1=mult_expr STAR e2=app_expr { BinOp(Mult, e1, e2) }
  | e=app_expr { e }

app_expr :
  | e1=app_expr e2=primary_expr { AppExp(e1,e2) }
  | FORK e=primary_expr { ForkExp e }
  | NEW LPAREN s=session RPAREN { NewExp s } (* 結合順位これで良い?? *)
  | SEND e=primary_expr f=primary_expr { SendExp(e,f) }
  | RECEIVE e=primary_expr { ReceiveExp e }
  | SELECT l=ID e=primary_expr { SelectExp (l,e) }
  | CLOSE e=primary_expr { CloseExp e }
  | WAIT e=primary_expr { WaitExp e }
  | e=primary_expr { e }


(* aexpr と同じ  *)
primary_expr :
  | LPAREN RPAREN { ULit }
  | v=INTV { ILit v }
  | TRUE { BLit true }
  | FALSE { BLit false }
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
  | BANG t=primary_ty PERIOD s=session { TySend(t,s) }
  | QU t=primary_ty PERIOD s=session { TyReceive(t,s) }

  | PLUS LBRACE br=separated_list(COMMA, branch_ty) RBRACE { TySelect br }
  | AMP LBRACE br=separated_list(COMMA, branch_ty) RBRACE { TyCase br }
  | s=primary_session { s }

branch_ty :
  | l=ID COLON s=session { (l,s) }

primary_session :
  | END BANG { TyClose }
  | END QU { TyWait }
  | HASH { TyDC }
  | LPAREN s=session RPAREN { s }

mult :
  | UN { Un }
  | LIN { Lin }
