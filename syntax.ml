(* To be removed: *)
exception Error of string
let err s = raise (Error s)
let todo () = err "Not implemented yet."

(* AST of the surface language *)

type id = string
type label = string

(* Types *********************************************)

(* multiplicity *)
type mult = Un | Lin

(* prefix は T だけでも十分か *)
(* mutual recursive types *)
type ty =
  (* TODO: TyBase にしてもいい？
   * ただ Unit は base type 以上の意味があるようにも *)
  | TyUnit
  | TyInt
  | TyBool
  | TySession of session
  | TyFun of mult * ty * ty
  (* TyPair でも良いかも ??? *)
  | TyProd of mult * ty * ty
  | TyDyn

(* session types *)
and session =
  | TySend of ty * session
  | TyReceive of ty * session
  | TySelect of (label * session) list
  | TyCase of (label * session) list
  | TyClose
  | TyWait
  | TyDC

let string_of_mult = function
  | Un -> "un"
  | Lin -> "lin"

(* TODO: カッコを減らす *)
let rec string_of_ty = function
  | TyUnit -> "unit"
  | TyInt -> "int"
  | TyBool -> "bool"
  | TySession s -> string_of_session s
  | TyFun (m,t,u) ->
     Printf.sprintf "(%s ->%s %s)"
       (string_of_ty t) (string_of_mult m) (string_of_ty u)
  | TyProd (m,t,u) ->
     Printf.sprintf "(%s *%s %s)"
       (string_of_ty t) (string_of_mult m) (string_of_ty u)
  | TyDyn -> "*"

and string_of_session = function
  | TySend (t,s) ->
     Printf.sprintf "(!%s.%s)"
       (string_of_ty t) (string_of_session s)
  | TyReceive (t,s) ->
     Printf.sprintf "(?%s.%s)"
       (string_of_ty t) (string_of_session s)
  | TySelect brs ->
     (* FIXME: 汚い *)
     let s =
       String.concat ", "
         (List.map (fun (l,s) -> l ^ ":" ^ string_of_session s) brs) in
     Printf.sprintf "+{%s}" s
  | TyCase brs ->
     let s =
       String.concat ", "
         (List.map (fun (l,s) -> l ^ ":" ^ string_of_session s) brs) in
     Printf.sprintf "&{%s}" s
  | TyClose -> "end!"
  | TyWait -> "end?"
  | TyDC -> "#"


(* duality *)
(* session -> session *)
let rec dual = function
  | TySend (t,s) -> TyReceive (t, dual s)
  | TyReceive (t,s) -> TySend (t, dual s)
  | TySelect brs ->
     TyCase (List.map (fun (l,s) -> (l, dual s)) brs)
  | TyCase brs ->
     TySelect (List.map (fun (l,s) -> (l, dual s)) brs)
  | TyClose -> TyWait
  | TyWait -> TyClose
  | TyDC -> TyDC

let mult_of_ty = function
  | TyUnit -> Un
  | TyInt -> Un
  | TyBool -> Un
  | TySession _ -> Lin          (* includes TyDC *)
  | TyFun (m, _, _) -> m
  | TyProd (m, _, _) -> m
  | TyDyn -> Un

(* m(T) *)
let lin ty = mult_of_ty ty = Lin
(* let un ty = not (lin ty) *)
let un ty = mult_of_ty ty = Un


(* multiplicity order *)
(* <= でも良い気もする。 *)
(* m <: n *)
let sub_mult m n = match m,n with
  | Un, _ -> true
  | Lin, Lin -> true
  | Lin, Un -> false


(* consistent subtyping *)
(* ty -> ty -> bool *)
(* let rec (<~) t u = match t,u with *)
let rec con_sub_ty t u = match t,u with
  (* | TyBase x, TyBase y when x = y -> true *)
  | TyUnit, TyUnit -> true
  | TyInt, TyInt -> true
  | TyBool, TyBool -> true
  | TySession s, TySession r -> con_sub_session s r
  | TyFun (m,t1,u1), TyFun (n,t2,u2) ->
     con_sub_ty t2 t1
     && con_sub_ty u1 u2
     && sub_mult m n
  | TyProd (m,t1,u1), TyProd (n,t2,u2) ->
     con_sub_ty t1 t2
     && con_sub_ty u1 u2
     && sub_mult m n
  | TyDyn, _ -> true
  | _, TyDyn -> true

  (* そもそも、t,uのコンストラクタが異なる等 *)
  | _ -> false

(* session -> session -> bool *)
and con_sub_session s r = match s,r with
  | TySend (t1,s1), TySend (t2,s2) ->
     con_sub_ty t2 t1
     && con_sub_session s1 s2
  | TyReceive (t1,s1), TyReceive (t2,s2) ->
     con_sub_ty t1 t2
     && con_sub_session s1 s2
  (* TODO: select,case が考えないといけない *)
  | TySelect _, TySelect _ -> todo ()
  | TyCase _, TyCase _ -> todo ()
  | TyClose, TyClose -> true
  | TyWait, TyWait -> true
  | TyDC, _ -> true
  | _, TyDC -> true
  | _ -> false



(* Programs *****************************************)

(* TODO:
 * mult という名前で良いか,
 * m は Fun, ConsPair のどの位置に置くか,
 * syntax はどうしたら自然か？
 * Fun, ConsPair について lin と un の2通り作ることも考えられる。アリかも
 * ただ m を推論したいんだったら？
 *
 * with_position
 * *)

(* NOTE:
 * コンストラクタの postfix の Exp は付けずに。
 * 代わりに prefix=E もありえる
 * *)

type binOp = Plus | Minus | Mult | Div (* | LT | Eq *)

type exp =
  | Var of id
  | UnitV
  | IntV of int
  | BoolV of bool
  (* integer, bool literal *)
  | BinOp of binOp * exp * exp

  (* fun m (x:T) -> e *)
  (* 規則では Abs *)
  | Fun of mult * id * ty * exp
  | App of exp * exp
  | PairCons of mult * exp * exp
  (* let x, y = e1 in e2 *)
  | PairDest of id * id * exp * exp
  | Fork of exp
  (* create both channel endpoints,
   * whose types are session & dual(session) *)
  | New of session
  | Send of exp * exp
  | Receive of exp
  | Select of label * exp
  (* FIXME: branch を置き換えよ *)
  | Case of exp * (label * id * exp) list
  | Close of exp
  | Wait of exp


type proc =
  | Exp of exp
  | Par of proc * proc
  | NuBind of id * id * proc
