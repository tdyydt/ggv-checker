

(* 型付けに用いた linear 変数の集合を返す。 *)
(* tyenv -> exp -> ty *)
let ty_exp tyenv = function
  (* | Name z -> *)
  | Const c -> ty_const c

  | Fun(m,x,t,e) ->
     let u = ty_exp (E.extend x t tyenv) e in
     if m :> tyenv then TyFun(m,t,u)
     else error ""
