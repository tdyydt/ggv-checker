open Syntax
(* module E = Environment *)

exception Typing_error of string
let ty_err s = raise (Typing_error s)


(* TODO: どのファイルに定義すべきか？
 * Syntax に移すかもしれない
 * translation 等でも使うため *)
(* TODO: ty_err じゃなくて、matching_err とかのほうがいい？ *)

(* matching_base ?????? *)
let matching_unit = function
  | TyUnit -> TyUnit
  | TyDyn -> TyUnit
  | _ -> ty_err "matching error: unit"

let matching_int = todo ()
let matching_bool = todo ()

let matching_fun = function
  | TyFun (m,t,u) as ty -> ty
  | TyDyn -> TyFun (Lin, TyDyn, TyDyn)
  | _ -> ty_err "matching error: fun"

let matching_prod = function
  | TyProd (m,t,u) as ty -> ty
  | TyDyn -> TyProd (Lin, TyDyn, TyDyn)
  | _ -> ty_err "matching error: prod"

(* ty -> ty *)
(* ty -> session も有り得る
 * 統一性の観点から、なし？ *)
let matching_send = function
  | TySession (TySend (t,s)) as ty -> ty
  | (TySession TyDC | TyDyn) -> TySession (TySend (TyDyn, TyDC))
  | _ -> ty_err "matching error: send"

let matching_receive = function
  | TySession (TyReceive (t,s)) as ty -> ty
  | (TySession TyDC | TyDyn) -> TySession (TyReceive (TyDyn, TyDC))
  | _ -> ty_err "matching error: receive"

let matching_select = function
  | TySession (TySelect _) as ty -> ty
  (* ラベル li と、添字集合 I の選び方？ *)
  (* select の場合は、 *)
  | (TySession TyDC | TyDyn) -> todo ()
     (* TySession (TySelect []) *)
  | _ -> ty_err "matching error: select"

let matching_case = function
  | TySession (TyCase _) as ty -> ty
  | (TySession TyDC | TyDyn) -> todo ()
  | _ -> ty_err "matching error: case"

let matching_close = function
  | TySession TyClose as ty -> ty
  | (TySession TyDC | TyDyn) -> TySession TyClose
  | _ -> ty_err "matching error: close"

let matching_wait = function
  | TySession TyWait as ty -> ty
  | (TySession TyDC | TyDyn) -> TySession TyWait
  | _ -> ty_err "matching error: wait"


(* 型付けに用いた linear 変数の集合を返す。 *)
(* tyenv -> exp -> ty *)
let rec ty_exp tyenv = function
  | Var x -> todo ()
  | Konst KUnit ->
     if un tyenv then TyUnit
     else ty_err "linearity error"
  | Konst (KInt _) -> TyInt
  | Konst (KBool _) -> TyBool

  | BinOp (op, e1, e2) ->
     let t1 = ty_exp tyenv e1 in
     let t2 = ty_exp tyenv e2 in
     (* op の種類に依る。
      * Lt とかもありえる
      * t1,t2 が int か確かめる等
      * LAnd 等も binop に入れて良いのでは？ *)
     TyInt
       (* or typing error *)

  (* TODO: env の実装 *)
(*
  | Fun (m,x,t,e) ->
     let u = ty_exp (E.extend x t tyenv) e in
     if m :> tyenv then TyFun (m,t,u)
     else ty_err ""
 *)

  | App (e1,e2) ->
     let t1 = ty_exp tyenv e1 in
     let t2 = ty_exp tyenv e2 in
     match matching_fun t1 with
     | TyFun (m, t11, t12) ->
        if con_sub_ty t2 t11 then t12
        else ty_err "not consistent subtype"
     (* matching fun とかで、すでに弾かれていて
      * ここまでエラーが来ないかも。
      * TODO: どう書くのが自然？ *)
     | _ -> assert false
