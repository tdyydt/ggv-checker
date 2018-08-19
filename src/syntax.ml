(* To be removed: *)
exception Error of string
let err s = raise (Error s)
let todo () = err "Not implemented yet."

open Printf

(* AST of the surface language *)

type id = string
type label = string

(*** Types ***)

(* multiplicity *)
type mult = Un | Lin

let string_of_mult = function
  | Un -> "un"
  | Lin -> "lin"

type ty =
  (* TODO: Add TyBase? *)
  | TyUnit
  | TyInt
  | TyBool
  | TySession of session
  | TyFun of mult * ty * ty
  (* OR: TyPair? *)
  | TyProd of mult * ty * ty
  | TyDyn                       (* dynamic type *)

(* session types *)
and session =
  | TySend of ty * session
  | TyReceive of ty * session
  | TySelect of (label * session) list
  | TyCase of (label * session) list
  | TyClose
  | TyWait
  | TyDC                        (* dynamic channel/session type *)

(* TODO: reduce paren *)
let rec string_of_ty = function
  | TyUnit -> "unit"
  | TyInt -> "int"
  | TyBool -> "bool"
  | TySession s -> string_of_session s
  | TyFun (m,t,u) ->
     sprintf "(%s ->%s %s)"
       (string_of_ty t) (string_of_mult m) (string_of_ty u)
  | TyProd (m,t,u) ->
     sprintf "(%s *%s %s)"
       (string_of_ty t) (string_of_mult m) (string_of_ty u)
  | TyDyn -> "*"

and string_of_session = function
  | TySend (t,s) ->
     sprintf "(!%s.%s)" (string_of_ty t) (string_of_session s)
  | TyReceive (t,s) ->
     sprintf "(?%s.%s)" (string_of_ty t) (string_of_session s)
  | TySelect brs ->
     sprintf "+{%s}"
       (String.concat ", "
          (List.map (fun (l,s) -> l ^ ":" ^ string_of_session s)
             brs))
  | TyCase brs ->
     sprintf "&{%s}"
       (String.concat ", "
          (List.map (fun (l,s) -> l ^ ":" ^ string_of_session s)
             brs))
  | TyClose -> "end!"
  | TyWait -> "end?"
  | TyDC -> "#"


(* duality *)
let rec dual : session -> session = function
  | TySend (t,s) -> TyReceive (t, dual s)
  | TyReceive (t,s) -> TySend (t, dual s)
  | TySelect brs ->
     TyCase (List.map (fun (l,s) -> (l, dual s)) brs)
  | TyCase brs ->
     TySelect (List.map (fun (l,s) -> (l, dual s)) brs)
  | TyClose -> TyWait
  | TyWait -> TyClose
  | TyDC -> TyDC

let mult_of_ty : ty -> mult = function
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


(* multiplicity order: [m <: n] *)
let sub_mult m n = match m,n with
  | Un, _ -> true
  | Lin, Lin -> true
  | Lin, Un -> false


(* consistent subtyping: [t <~ u] *)
let rec con_sub_ty (t : ty) (u : ty) : bool = match t,u with
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
  (* e.g.) t,u has different type constructors *)
  | _ -> false

(* [s <~ r] *)
and con_sub_session (s : session) (r : session) : bool =
  match s,r with
  | TySend (t1,s1), TySend (t2,s2) ->
     con_sub_ty t2 t1
     && con_sub_session s1 s2
  | TyReceive (t1,s1), TyReceive (t2,s2) ->
     con_sub_ty t1 t2
     && con_sub_session s1 s2
  (* TODO: select,case が考えないといけない *)
  | TySelect brs1, TySelect brs2 -> todo ()
  | TyCase _, TyCase _ -> todo ()
  | TyClose, TyClose -> true
  | TyWait, TyWait -> true
  | TyDC, _ -> true
  | _, TyDC -> true
  | _ -> false

(*** Programs ***)

(* NOTE: Where should I put m in Fun, PairCons?
 * It is possible to use two constructors: for each lin and un
 * but I want to infer m in the future. *)

type binOp = Plus | Minus | Mult | Div (* | LT | Eq *)

(* TODO: postfix Exp? *)
type exp =
  | Var of id
  | ULit                        (* unit: () *)
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp

  (* OR: Abs *)
  | FunExp of mult * id * ty * exp   (* \m (x:T) -> e *)
  | AppExp of exp * exp
  | LetExp of id * exp * exp       (* let x = e1 in e2 *)

  | PairCons of mult * exp * exp
  (* let x, y = e1 in e2 *)
  | PairDest of id * id * exp * exp
  | ForkExp of exp
  (* create both channel endpoints,
   * whose types are session & dual(session) *)
  | NewExp of session
  | SendExp of exp * exp
  | ReceiveExp of exp
  | SelectExp of label * exp
  (* FIXME: branch を置き換えよ *)
  | CaseExp of exp * (label * id * exp) list
  | CloseExp of exp
  | WaitExp of exp


type proc =
  | Exp of exp
  | Par of proc * proc
  (* (nu (c:S),d) P *)
  | NuBind of id * id * session* proc
