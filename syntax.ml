(* AST of the surface language *)

type id = string
type label = string

(* types *********************************************)

(* multiplicity *)
type mult = Un | Lin

(* prefix は T だけでも十分か *)
type ty =
  | TyUnit
  | TyInt
  | TyBool
  | TySession of session
  | TyFun of mult * ty * ty
  (* TyPair ??? *)
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


(* programs *****************************************)

(* TODO:
 * mult という名前で良いか,
 * m は Fun, ConsPair のどの位置に置くか,
 * syntax はどうしたら自然か？
 * Fun, ConsPair について lin と un の2通り作ることも考えられる。アリかも
 * ただ m を推論したいんだったら？
 *
 * with_position
 * *)

(* 区別しなくて良いかも？ *)
(* type name =
 *   | Var of id
 *   | Chan of id *)
(* パースの時に区別する術がない *)

(* konst *)
type konst =
  | KUnit
  | KInt of int
  | KBool of bool

(* NOTE:
 * コンストラクタの postfix の Exp は付けずに。
 * *)

type binOp = Plus | Mult

type exp =
  | Var of id
  (* | Unit *)
  | Konst of konst
  (* integer, bool literal *)
  | BinOp of binOp * exp * exp

  (* fun x:T -> e *)
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
