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


(* TODO: 型付けに用いた linear 変数の集合を返す。
 * ように改良する *)
(* tyenv -> exp -> ty *)
let rec ty_exp tyenv = function
  | Var x -> todo ()
  | Konst KUnit ->
     if mult_tyenv Un tyenv then TyUnit
     else ty_err "violation of linearity"
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
  | Fun (Lin,x,t,e) ->
     let u = ty_exp (todo ()) e in
     TyFun (Lin,t,u)
  | Fun (Un,x,t,e) ->
     let u = ty_exp (todo ()) e in
     if mult_tyenv Un tyenv then TyFun (Un,t,u)
     (* gamma に入っているだけで、実際に使われているかは、言い切れない *)
     else ty_err "unrestricted functions cannot contain variables of a linear type"

  | App (e1,e2) ->
     let t1 = ty_exp tyenv e1 in
     let t2 = ty_exp tyenv e2 in
     begin
       match matching_fun t1 with
       | TyFun (m, t11, t12) ->
          if con_sub_ty t2 t11 then t12
          else ty_err "not consistent subtype"
       (* matching fun とかで、すでに弾かれていて
        * ここまでエラーが来ないかも。
        * TODO: どう書くのが自然？ *)
       | _ -> assert false
     end

  (* TODO: @ じゃなくて、set_union になるべきなのかもしれん。
   * そうではない, disjoint であることを、先に確かめているため *)
  | ConsPair (Lin,e1,e2) ->
     let t1, vars1 = ty_exp tyenv e1 in
     let t2, vars2 = ty_exp tyenv e2 in
     (* assert_disjoint vars1 vars2; *)
     (TyProd (Lin,t1,t2), vars1 @@ vars2)

  | ConsPair (Un,e1,e2) ->
     let t1, vars1 = ty_exp tyenv e1 in
     let t2, vars2 = ty_exp tyenv e2 in
     (* assert_disjoint vars1 vars2 *)
     if mult_of_ty t1 = Un && mult_of_ty t2 = Un
     then (TyProd (Un,t1,t2), vars1 @@ vars2)
     else ty_err "unrestricted pairs cannot contain linear varialbes"

  (* f は使わないべき？？統一性のため *)
  | DestPair (x,t1,y,t2,e1,e2) ->
     let t1, vars1 = ty_exp tyenv e1 in
     (* extend (x,t1) (y,t2) in tyenv *)
     todo ()

  | Fork e ->
     let t, vars = ty_exp tyenv e in
     begin
       match matching_unit t with
       | TyUnit -> (TyUnit, vars)
       | _ -> assert false
     end

  | New -> todo ()

  | Send (e1,e2) ->
     let t1, vars1 = ty_exp tyenv e1 in
     let t2, vars2 = ty_exp tyenv e2 in
     (* assert_disjoint vars1 vars2; *)
     begin
       match matching_send t2 with
       | TySession (TySend (t3,s)) ->
          if con_sub_ty t1 t3
          then (TySession s, vars1 @@ vars2)
          else ty_err "Not consistent subtype"
       | _ -> assert false
     end

  | Receive e -> todo ()
  | Select l e -> todo ()
  (* | Case ??? -> todo () *)
  | Close e -> todo ()
  | Wait e -> todo ()
