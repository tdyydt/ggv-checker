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

let string_of_ty = function
  | TyUnit -> "unit"
  | TyInt -> "int"
  | TyBool -> "bool"


(* duality *)
(* ty じゃなくて、session だけで呈される。 *)
let dual = todo ()

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

(* check if m(tyenv) holds *)
(* TODO: better name ?? *)
let mult_tyenv m tyenv =
  (* tyenv から tys だけ取り出す。 *)
  let tys = todo () in
  List.for_all (fun t -> mult_of_ty t = m) tys

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

(* konst *)
type konst =
  | KUnit
  | KInt of int
  | KBool of bool

(* NOTE:
 * コンストラクタの postfix の Exp は付けずに。
 * 代わりに prefix=E もありえる
 * *)

type binOp = Plus | Mult

type exp =
  | Var of id
  (* | Unit *)
  | Konst of konst
  (* integer, bool literal *)
  | BinOp of binOp * exp * exp

  (* fun x:T -> e *)
  (* 規則では Abs *)
  | Fun of mult * id * ty * exp
  | App of exp * exp
  | ConsPair of mult * exp * exp
  (* let x:T1, y:T2 = e1 in e2 *)
  | DestPair of id * ty * id * ty * exp * exp
  | Fork of exp
  (* TODO: newするチャネルの型が必要？ *)
  | New
  | Send of exp * exp
  | Receive of exp
  | Select of label * exp
  (* FIXME: branch を置き換えよ *)
  (* | Case of exp * (??? list) *)
  | Close of exp
  | Wait of exp


type proc =
  | Exp of exp
  | Par of proc * proc
  | NuBind of id * id * proc
