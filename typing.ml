open Syntax

(* 型付けに用いた linear 変数の集合を返す。 *)
(* tyenv -> exp -> ty *)
let ty_exp tyenv = function
  (* | Name z -> *)
  | Konst KUnit -> TyUnit
  | Konst (KInt _) -> TyInt
  | Konst (KBool _) -> TyBool

  (* | Fun(m,x,t,e) ->
   *    let u = ty_exp (E.extend x t tyenv) e in
   *    if m :> tyenv then TyFun(m,t,u)
   *    else error "" *)
