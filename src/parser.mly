%{
open Syntax
%}

%token SEMISEMI
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LIN UN
%token UNIT INT BOOL
%token BANG QU AMP HASH
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
%left STAR SLASH                (* mult,div *)

%start toplevel
%type <Syntax.exp> toplevel
%%

toplevel :
  | e=expr SEMISEMI { e }

expr :
  | e1=expr op=binop e2=expr { BinOp (op, e1, e2) }
  | FUN m=mult LPAREN x=ID t=ty_annot RPAREN RARROW e=expr %prec prec_fun
    { FunExp(m,x,t,e) }
  | LET x=ID COMMA y=ID EQ e1=expr IN e2=expr %prec prec_let
    { PairDest(x,y,e1,e2) }
  | LET x=ID EQ e1=expr IN e2=expr %prec prec_let
    { LetExp (x,e1,e2) }

  | CASE e=expr OF
    LBRACE br=separated_nonempty_list(SEMI, branch) RBRACE
    { CaseExp (e,br) }

  (* TODO: Where should I put mult?? *)
  | LPAREN e1=expr COMMA e2=expr RPAREN m=mult
    { PairCons(m,e1,e2) }

  | FORK e=simple_expr { ForkExp e }
  | NEW LPAREN s=session RPAREN { NewExp s }
  | SEND e1=simple_expr e2=simple_expr { SendExp(e1,e2) }
  | RECEIVE e=simple_expr { ReceiveExp e }
  | SELECT l=ID e=simple_expr { SelectExp (l,e) }
  | CLOSE e=simple_expr { CloseExp e }
  | WAIT e=simple_expr { WaitExp e }
  | e=minus_expr { e }

%inline binop :
  | PLUS { Plus }               (* arith *)
  | MINUS { Minus }
  | STAR { Mult }
  | SLASH { Div }

(* case branch *)
branch:
  (* | l=ID COLON x=ID PERIOD e=expr { (l,x,e) } *)
  (* | l=ID COLON LPAREN x=ID t=ty_annot RPAREN PERIOD e=expr *)
  | l=ID COLON x=ID PERIOD e=expr { (l,x,e) }

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
  | v=INTV { ILit v }
  | TRUE { BLit true }
  | FALSE { BLit false }
  | x=ID { Var x }
  | LPAREN e=expr RPAREN { e }


(* type annotation  *)
ty_annot :
  | COLON t=ty { t }

ty :
 (* TODO: How should I put m?  *)
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
