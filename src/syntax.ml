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
  | TyDyn -> "Dyn"

and string_of_session = function
  | TySend (t,s) ->
     sprintf "(!%s.%s)" (string_of_ty t) (string_of_session s)
  | TyReceive (t,s) ->
     sprintf "(?%s.%s)" (string_of_ty t) (string_of_session s)
  | TySelect choices ->
     sprintf "+{%s}"
       (String.concat ", "
          (List.map (fun (l,s) -> l ^ ":" ^ string_of_session s)
             choices))
  | TyCase choices ->
     sprintf "&{%s}"
       (String.concat ", "
          (List.map (fun (l,s) -> l ^ ":" ^ string_of_session s)
             choices))
  | TyClose -> "end!"
  | TyWait -> "end?"
  | TyDC -> "DC"

(*** Programs ***)

(* NOTE: Where should I put m in Fun, PairCons?
 * It is possible to use two constructors: for each lin and un
 * but I want to infer m in the future. *)

type binOp = Plus | Minus | Mult | Div | Lt | Gt | Eq | LE | GE

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
  | CaseExp of exp * (label * id * session * exp) list
  | CloseExp of exp
  | WaitExp of exp

(* program *)
type prog = Exp of exp
