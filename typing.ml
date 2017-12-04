open Syntax
module E = Environment

type tyenv = ty E.t

(* set of variable *)
(* IdSet, VarSet Variables *)
module VarSet =
  Set.Make(
      struct
        type t = id
        let compare = compare
      end
    )

exception Typing_error of string
let ty_err s = raise (Typing_error s)


(**************************************************)
(* TODO: どのファイルに定義すべきか？
 * Syntax に移すかもしれない
 * translation 等でも使うため *)
(* TODO: ty_err じゃなくて、matching_err とかのほうがいい？ *)

(* matching_base ?????? *)
let matching_unit = function
  | TyUnit -> TyUnit
  | TyDyn -> TyUnit
  | _ -> ty_err "matching error: unit"

let matching_int = function
  | (TyInt | TyDyn) -> TyInt
  | _ -> ty_err "matching error: int"

let matching_bool = function
  | (TyBool | TyDyn) -> TyBool
  | _ -> ty_err "matching error: bool"

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

let matching_select t l = match t with
  | TySession (TySelect br) -> br
  (* TySelect {l : TyDC } *)
  | (TySession TyDC | TyDyn) -> [(l, TyDC)]
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


(**************************************************)
(* TODO: better name ?? *)
(* let mult_tyenv m tyenv =
 *   (\* tyenv から tys だけ取り出す。 *\)
 *   let tys = todo () in
 *   List.for_all (fun t -> mult_of_ty t = m) tys *)

(* Env を参照する、どこで定義すべきか？？ *)
(* check if m(tyenv) holds *)
let un_tyenv tyenv =
  (* tyenv から tys だけ取り出す。 *)
  let tys = Environment.values tyenv in
  List.for_all un tys

(* unだけで、linは使わない？ *)
let lin_tyenv tyenv =
  let tys = Environment.values tyenv in
  List.for_all lin tys


let assert_disjoint xs ys =
  let zs = VarSet.inter xs ys in
  if VarSet.is_empty zs then ()
                               (* zs を表示すれば良さそう。 *)
                               (* violation of linearity *)
                               (* 関数の名前から、specific すぎるエラーはどうなのか？ *)
  else ty_err "Not disjoint sets"
(* VarSet.t -> VarSet.t -> bool *)
(* let is_disjoint xs ys =
 *   VarSet.is_empty (VarSet.inter xs ys) *)


(* TODO:
 * linear な変数が使われずに、捨てられることをどこで咎める？？
 * un_tyenv tyenv にあたるもの。
 * un_tyenv は要らない？
 * Gamma から lin vars だけ取り出す処理が必要か？？
 *
 * Gamma はずっと連れ回されて、
 * 最後の 式e がプロセスになる時点で
 * lin_vars(tyenv) = eで使われるlv集合
 *
 * process typing のところでの判定になる？
 * この判定がどこにも見えない。
 * *)

(* returns the set of linear type variables used in typing *)
(* tyenv -> exp -> ty * VarSet.t *)
let rec ty_exp tyenv = function
  | Var x -> begin
      try
        let t = E.find x tyenv in
        if lin t then (t, VarSet.singleton x)
        else (t, VarSet.empty)
      with
      | Not_found -> ty_err ("the variable " ^ x ^ " is not bound")
    end
  | UnitV -> (TyUnit, VarSet.empty)
  | IntV _ -> (TyInt, VarSet.empty)
  | BoolV _ -> (TyBool, VarSet.empty)

  (* TODO: fix, consider LT *)
  (* rename? arithmetic operation ?? *)
  | BinOp (op, e1, e2) ->
     let t1, xs = ty_exp tyenv e1 in
     let t2, ys = ty_exp tyenv e2 in
     assert_disjoint xs ys;
     begin
       match matching_int t1, matching_int t2 with
       | TyInt, TyInt -> (TyInt, VarSet.union xs ys)
       | _ -> assert false
     end
     (* TODO: un_tyenv tyenv ?? が必要か？？？？*)

     (* op の種類に依る。
      * Lt とかもありえる
      * t1,t2 が int か確かめる等
      * LAnd 等も binop に入れて良いのでは？ *)

  (* | Fun (m,x,t,e) ->
   *    let u, ys = ty_exp (E.add x t tyenv) e in
   *    (\* m :> (Gamma) *\)
   *    if m = Lin || un_tyenv tyenv
   *      (\* remove x from ys, if included *\)
   *    then (TyFun (m,t,u), VarSet.remove x ys)
   *    (\* gamma に入っているだけで、実際に使われているかは、言い切れない *\)
   *    else ty_err "unrestricted functions cannot contain variables of a linear type" *)

  | Fun (Lin,x,t,e) ->
     let u, ys = ty_exp (E.add x t tyenv) e in
     (TyFun (Lin,t,u), VarSet.remove x ys)

  | Fun (Un,x,t,e) ->
     (* un 関数を作るため、
      * e に lin 変数が含まれていないこと *)
     let u, ys = ty_exp (E.add x t tyenv) e in
     if VarSet.is_empty ys
     then (TyFun (Un,t,u), VarSet.remove x ys)
     else ty_err "unrestricted functions cannot contain variables of a linear type"

  | App (e1,e2) ->
     let t1, xs = ty_exp tyenv e1 in
     let t2, ys = ty_exp tyenv e2 in
     begin
       match matching_fun t1 with
       | TyFun (m, t11, t12) ->
          if con_sub_ty t2 t11
          then (t12, VarSet.union xs ys)
          else ty_err "not consistent subtype: app"
       (* matching fun とかで、すでに弾かれていて
        * ここまでエラーが来ないかも。
        * TODO: どう書くのが自然？ *)
       | _ -> assert false
     end

  (* m = Un であったら、
   * xs,ysはから出ないといけない。*)
  (* | PairCons (m,e1,e2) ->
   *    let t1, xs = ty_exp tyenv e1 in
   *    let t2, ys = ty_exp tyenv e2 in
   *    assert_disjoint xs ys;
   *    (\* m :> (t1) /\ m :> (t2) *\)
   *    if m = Lin || (un t1 && un t2)
   *    then (TyProd (m,t1,t2), VarSet.union xs ys)
   *    else ty_err "unrestricted pairs cannot contain linear varialbes" *)

  | PairCons (Lin,e1,e2) ->
     let t1, xs = ty_exp tyenv e1 in
     let t2, ys = ty_exp tyenv e2 in
     assert_disjoint xs ys;
     (TyProd (Lin,t1,t2), VarSet.union xs ys)

  | PairCons (Un,e1,e2) ->
     let t1, xs = ty_exp tyenv e1 in
     let t2, ys = ty_exp tyenv e2 in
     assert_disjoint xs ys;
     (* e1,e2 に linear 変数が含まれている場合、
      * それらが複製されることになる *)
     if VarSet.is_empty xs && VarSet.is_empty ys
     then (TyProd (Un,t1,t2), VarSet.union xs ys)
     else ty_err "unrestricted pairs cannot contain linear varialbes"

  (* | PairDest (x1,t1,x2,t2,e,f) ->
   *    let t, ys = ty_exp tyenv e in
   *    (\* extend (x,t1) (y,t2) in tyenv *\)
   *    let u, zs = ty_exp (E.add x1 t1 (E.add x2 t2 tyenv)) f in
   *    let zs' = VarSet.remove x1 (VarSet.remove x2 zs) in
   *    assert_disjoint ys zs';
   *    (u, VarSet.union ys zs') *)

  | PairDest (x1,x2,e,f) ->
     let t, ys = ty_exp tyenv e in
     begin
       match matching_prod t with
       | TyProd (_, t1,t2) ->
          let u, zs = ty_exp (E.add x1 t1 (E.add x2 t2 tyenv)) f in
          let zs' = VarSet.remove x1 (VarSet.remove x2 zs) in
          assert_disjoint ys zs';
          (u, VarSet.union ys zs')
       | _ -> assert false
     end

  | Fork e ->
     let t, xs = ty_exp tyenv e in
     begin
       match matching_unit t with
       | TyUnit -> (TyUnit, xs)
       | _ -> assert false
     end

  | New s ->
     (TyProd (Lin, TySession s, TySession (dual s)),
      VarSet.empty)

  | Send (e1,e2) ->
     let t1, xs = ty_exp tyenv e1 in
     let t2, ys = ty_exp tyenv e2 in
     assert_disjoint xs ys;
     begin
       match matching_send t2 with
       | TySession (TySend (t3,s)) ->
          if con_sub_ty t1 t3
          then (TySession s, VarSet.union xs ys)
          else ty_err "Not consistent subtype: send"
       | _ -> assert false
     end

  | Receive e ->
     let t1, xs = ty_exp tyenv e in
     begin
       match matching_receive t1 with
       | TySession (TyReceive (t2,s)) ->
          (TyProd (Lin, t2, TySession s), xs)
       | _ -> assert false
     end

  | Select (l,e) ->
     let t, xs = ty_exp tyenv e in
     let br = matching_select t l in
     (* check if label l is in branches *)
     (* l が入っているか？ *)
     (* if List.mem_assoc l br then *)
     begin
       try
         let s = List.assoc l br in (TySession s, xs)
       with
       | Not_found ->
          (* l が入っていないということ *)
          ty_err "label is not in branches"
     end

  (* br=branch *)
  | Case (e, brs) -> todo ()

  | Close e ->
     let t, xs = ty_exp tyenv e in
     begin
       match matching_close t with
       | TySession TyClose -> (TyUnit, xs)
       | _ -> assert false
     end

  | Wait e ->
     let t, xs = ty_exp tyenv e in
     begin
       match matching_wait t with
       | TySession TyWait -> (TyUnit, xs)
       | _ -> assert false
     end
