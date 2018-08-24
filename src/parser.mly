%{
open Syntax
%}

%token SEMISEMI
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LIN UN
%token UNIT INT BOOL DYN
%token BANG QU AMP DC
%token END PERIOD COMMA
%token PLUS MINUS STAR SLASH
(* %token LT *)
%token TRUE FALSE
%token LET IN EQ
%token FUN RARROW COLON
%token FORK NEW SEND RECEIVE
%token SELECT CLOSE WAIT
%token CASE OF SEMI

%token <int> INTV
%token <Syntax.id> ID

(* precedence: lower to higher *)
(* via: https://caml.inria.fr/pub/docs/manual-ocaml/expr.html *)
%right prec_let prec_fun
(* %right prec_if *)
(* %left LT GT EQ LE GE *)
%left PLUS MINUS
%right RARROW                   (* function ty *)
%left STAR SLASH                (* mult,div *)

%start toplevel
%type <Syntax.prog> toplevel
%%

toplevel :
  | e=expr SEMISEMI { Exp e }

expr :
  | e1=expr op=binop e2=expr { BinOp (op, e1, e2) }
  | FUN m=mult p=para RARROW e=expr %prec prec_fun
    { let (x,t) = p in FunExp(m,x,t,e) }

  | LET x=ID EQ e1=expr IN e2=expr %prec prec_let
    { LetExp (x,e1,e2) }
  | LET x=ID COMMA y=ID EQ e1=expr IN e2=expr %prec prec_let
    { PairDest(x,y,e1,e2) }
  (* TODO: Where should I put mult?? *)
  | LPAREN e1=expr COMMA e2=expr RPAREN m=mult
    { PairCons(m,e1,e2) }

  | FORK e=simple_expr { ForkExp e }
  | NEW s=simple_session { NewExp s }
  | SEND e1=simple_expr e2=simple_expr { SendExp(e1,e2) }
  | RECEIVE e=simple_expr { ReceiveExp e }
  | SELECT l=ID e=simple_expr { SelectExp (l,e) }
  | CASE e=expr OF
    LBRACE br=separated_nonempty_list(SEMI, branch) RBRACE
    { CaseExp (e,br) }
  | CLOSE e=simple_expr { CloseExp e }
  | WAIT e=simple_expr { WaitExp e }
  | e=minus_expr { e }

para :
  | LPAREN x=ID COLON t=ty RPAREN { (x,t) }
  | LPAREN x=ID COLON s=session RPAREN { (x, TySession s) }

%inline binop :
  | PLUS { Plus }               (* arith *)
  | MINUS { Minus }
  | STAR { Mult }
  | SLASH { Div }

(* case branch *)
branch:
  (* TODO: PERIOD to RARROW ? *)
  (* l(x:S).e *)
  | l=ID LPAREN x=ID COLON s=session RPAREN PERIOD e=expr
    { (l,x,s,e) }
  (* add type annotation S *)
  (* | l=ID COLON LPAREN x=ID t=ty_annot RPAREN PERIOD e=expr *)

minus_expr :
  | MINUS e=minus_expr
    { match e with
      | ILit n -> ILit (-n)
      | e -> BinOp (Minus, ILit 0, e) }
  | e=app_expr { e }

app_expr :
  | e1=app_expr e2=simple_expr { AppExp(e1,e2) }
  | e=simple_expr { e }

simple_expr :
  | LPAREN RPAREN { ULit }
  | n=INTV { ILit n }
  | TRUE { BLit true }
  | FALSE { BLit false }
  | x=ID { Var x }
  | LPAREN e=expr RPAREN { e }

ty :
 (* TODO: How should I put m?  *)
  | t1=ty RARROW m=mult t2=ty { TyFun(m,t1,t2) }
  | t1=ty STAR m=mult t2=ty { TyProd(m,t1,t2) }
  (* | s=session { TySession s } ==> cause conflict *)
  | t=simple_ty { t }

simple_ty :
  | UNIT { TyUnit }
  | INT { TyInt }
  | BOOL { TyBool }
  | DYN { TyDyn }
  | LPAREN t=ty RPAREN { t }

session :
  | BANG t=simple_ty PERIOD s=session { TySend(t,s) }
  | QU t=simple_ty PERIOD s=session { TyReceive(t,s) }

  | PLUS LBRACE br=separated_list(COMMA, branch_ty) RBRACE
    { TySelect br }
  | AMP LBRACE br=separated_list(COMMA, branch_ty) RBRACE
    { TyCase br }
  | s=simple_session { s }

branch_ty :
  | l=ID COLON s=session { (l,s) }

simple_session :
  | END BANG { TyClose }
  | END QU { TyWait }
  | DC { TyDC }
  | LPAREN s=session RPAREN { s }

(* Multiplicity *)
mult :
  | UN { Un }
  | LIN { Lin }
