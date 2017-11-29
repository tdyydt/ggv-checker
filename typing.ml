open Syntax
(* module E = Environment *)

exception Typing_error of string
let ty_err s = raise (Typing_error s)


(* TODO: どのファイルに定義すべきか？
 * Syntax に移すかもしれない
 * translation 等でも使うため *)
let matching_fun = function
  | TyFun (m,t,u) as ty -> ty
  | TyDyn -> TyFun (Lin, TyDyn, TyDyn)
  | _ -> ty_err "matching error"

let matching_send = function
  | TySession (TySend (t,s)) as ty -> ty
  | (TySession TyDC | TyDyn) -> TySession (TySend (TyDyn, TySession TyDC))
  | _ -> ty_err "matching error"

let matching_receive = function
  | TySession (TyReceive (t,s)) as ty -> ty


(* 型付けに用いた linear 変数の集合を返す。 *)
(* tyenv -> exp -> ty *)
let ty_exp tyenv = function
  (* | Var x -> not_implement *)
  | Konst KUnit -> TyUnit
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
        if t2 <~ t11 then t12
        else ty_err "not consistent subtype"
     (* matching fun とかで、すでに弾かれていて
      * ここまでエラーが来ないかも。
      * TODO: どう書くのが自然？ *)
     | _ -> ty_err ""
