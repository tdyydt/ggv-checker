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

(* 型を返すのではなく、
 * 型コンストラクタの引数を返す。
 * 引数が無い時は () を返す *)

(* matching_base ?????? *)
let matching_unit = function
  | (TyUnit | TyDyn) -> ()
  | _ -> ty_err "matching error: unit"

let matching_int = function
  | (TyInt | TyDyn) -> ()
  | _ -> ty_err "matching error: int"

let matching_bool = function
  | (TyBool | TyDyn) -> ()
  | _ -> ty_err "matching error: bool"

let matching_fun = function
  | TyFun (m,t,u) -> (m,t,u)
  | TyDyn -> (Lin, TyDyn, TyDyn)
  | _ -> ty_err "matching error: fun"

let matching_prod = function
  | TyProd (m,t,u) -> (m,t,u)
  | TyDyn -> (Lin, TyDyn, TyDyn)
  | _ -> ty_err "matching error: prod"

(* ty -> ty * session *)
let matching_send = function
  | TySession (TySend (t,s)) -> (t,s)
  | (TySession TyDC | TyDyn) -> (TyDyn, TyDC)
  | _ -> ty_err "matching error: send"

let matching_receive = function
  | TySession (TyReceive (t,s)) -> (t,s)
  | (TySession TyDC | TyDyn) -> (TyDyn, TyDC)
  | _ -> ty_err "matching error: receive"

let matching_select t l = match t with
  | TySession (TySelect br) -> br
  (* TySelect {l : TyDC } *)
  | (TySession TyDC | TyDyn) -> [(l, TyDC)]
  | _ -> ty_err "matching error: select"

let matching_case t ls = match t with
  | TySession (TyCase br) -> br
  | (TySession TyDC | TyDyn) ->
     List.map (fun l -> (l, TyDC)) ls
  | _ -> ty_err "matching error: case"

let matching_close = function
  | TySession TyClose -> ()
  | (TySession TyDC | TyDyn) -> ()
  | _ -> ty_err "matching error: close"

let matching_wait = function
  | TySession TyWait -> ()
  | (TySession TyDC | TyDyn) -> ()
  | _ -> ty_err "matching error: wait"


(**************************************************)
(* TODO: better name ?? *)
(* let mult_tyenv m tyenv =
 *   (\* tyenv から tys だけ取り出す。 *\)
 *   let tys = todo () in
 *   List.for_all (fun t -> mult_of_ty t = m) tys *)

(*
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
*)

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

  (* rename? arithmetic operation ?? *)
  | BinOp (op, e1, e2) ->
     let t1, xs = ty_exp tyenv e1 in
     let t2, ys = ty_exp tyenv e2 in
     assert_disjoint xs ys;
     let _ = matching_int t1, matching_int t2 in
     (TyInt, VarSet.union xs ys)

  (* TODO: LT 等も入れるか？？ if を入れる時で良さそう。 *)
     (* op の種類に依る。
      * Lt とかもありえる
      * t1,t2 が int か確かめる等
      * LAnd 等も binop に入れて良いのでは？ *)

  | Fun (Lin,x,t,e) ->
     (* 変数名の上書きを(一時的に)禁止 *)
     assert (not (E.mem x tyenv));

     let u, ys = ty_exp (E.add x t tyenv) e in
     (TyFun (Lin,t,u), VarSet.remove x ys)

  | Fun (Un,x,t,e) ->
     (* 変数名の上書きを(一時的に)禁止 *)
     assert (not (E.mem x tyenv));

     (* un 関数を作るため、
      * e に lin 変数が含まれていないこと *)
     let u, ys = ty_exp (E.add x t tyenv) e in
     if VarSet.is_empty ys
     then (TyFun (Un,t,u), VarSet.remove x ys)
     else ty_err "unrestricted functions cannot contain variables of a linear type"

  | App (e1,e2) ->
     let t1, xs = ty_exp tyenv e1 in
     let t2, ys = ty_exp tyenv e2 in
     let m, t11, t12 = matching_fun t1 in
     if con_sub_ty t2 t11
     then (t12, VarSet.union xs ys)
     else ty_err "not consistent subtype: app"

  | Let (x,e,f) ->
     (* 変数名の上書きを(一時的に)禁止 *)
     assert (not (E.mem x tyenv));

     let t, xs = ty_exp tyenv e in
     let u, ys = ty_exp (E.add x t tyenv) f in
     (* lin(t) の場合、 ys の中に x が入っているべき *)
     if lin t && not (VarSet.mem x ys)
                     (* x が linear なのに使われない *)
     then ty_err ("linear variable is not used: " ^ x)
     else (u, VarSet.remove x ys)

  | PairCons (Lin,e1,e2) ->
     let t1, xs = ty_exp tyenv e1 in
     let t2, ys = ty_exp tyenv e2 in
     assert_disjoint xs ys;
     (TyProd (Lin,t1,t2), VarSet.union xs ys)

  (* m = Un であったら、
   * xs,ysは空でないといけない。*)
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
     (* TODO: x1,x2 が既に定義されてる変数で、上書きの場合
      * remove の辺りが不十分か *)
     (* ===> *)
     (* 変数名の上書きを(一時的に)禁止 *)
     assert (not (E.mem x1 tyenv));
     assert (not (E.mem x2 tyenv));

     let t, ys = ty_exp tyenv e in
     let _, t1, t2 = matching_prod t in
     let u, zs = ty_exp (E.add x1 t1 (E.add x2 t2 tyenv)) f in

     (* x1,x2 のうち linear なものは、全て使われていないとダメ *)
     if lin t1 && not (VarSet.mem x1 zs)
     then ty_err ("linear variable is not used: " ^ x1)
     else if lin t2 && not (VarSet.mem x2 zs)
     then ty_err ("linear variable is not used: " ^ x2)
     else
       (* remove x1, x2 *)
       let zs' = VarSet.remove x1 (VarSet.remove x2 zs) in
       assert_disjoint ys zs';
       (u, VarSet.union ys zs')

  | Fork e ->
     let t, xs = ty_exp tyenv e in
     let _ = matching_unit t in
     (* assert VarSet.is_empty xs ?? *)
     if VarSet.is_empty xs
     then (TyUnit, xs)
     else ty_err "cannot contain linear variables: fork"

  | New s ->
     (TyProd (Lin, TySession s, TySession (dual s)),
      VarSet.empty)

  | Send (e1,e2) ->
     let t1, xs = ty_exp tyenv e1 in
     let t2, ys = ty_exp tyenv e2 in
     assert_disjoint xs ys;
     let t3, s = matching_send t2 in
     if con_sub_ty t1 t3
     then (TySession s, VarSet.union xs ys)
     else ty_err "Not consistent subtype: send"

  | Receive e ->
     let t1, xs = ty_exp tyenv e in
     let t2, s = matching_receive t1 in
     (TyProd (Lin, t2, TySession s), xs)

  | Select (l,e) ->
     let t, xs = ty_exp tyenv e in
     let br = matching_select t l in
     begin
       try
       (* check if label l is in branches *)
       (* l が入っているか？ *)
       (* この検査まで matching_select でできるが、
        * 規則に合わせると、そうしない方が自然か *)
         let s = List.assoc l br in (TySession s, xs)
       with
       (* l が入っていないということ *)
       | Not_found -> ty_err "label is not in branches"
     end

  (* br=branch *)
  | Case (e, brs) ->
     let t, xs = ty_exp tyenv e in
     (* pick up labels *)
     let ls = List.map (fun (l,_,_) -> l) brs in
     let ty_brs = matching_case t ls in
     begin
       try
         (* 式 f[i] の型 u[i],
          * その型付けで用いた linear 変数集合 ys[i]
          * の組たちのリスト *)
         let _ =
           List.map (fun (l,x,f) ->
               let s = List.assoc l ty_brs in
               let u, ys = ty_exp (E.add x (TySession s) tyenv) f in

               (* TODO: x は linear変数なので、使われないといけない *)

               (u, VarSet.remove x ys))
             brs in
         (* ys[i] たちは bigUnion を取る。
          * u[i] たちは等しいはず
          * assertEq か？ *)
         todo ()
       with
       (* どこかの部分式でエラーが起きた場合 *)
       | Not_found -> ty_err "label is not in branches"
     end


  | Close e ->
     let t, xs = ty_exp tyenv e in
     let () = matching_close t in
     (TyUnit, xs)

  | Wait e ->
     let t, xs = ty_exp tyenv e in
     let () = matching_wait t in
     (TyUnit, xs)


(* well-typed or ill-typed (bool) *)
(* ill-typed だったらエラーを投げて、
 * well-typed だったら unit でも返す？ *)
(* used vars も返すならば、unit相当は不要になる *)

(* proc -> tyenv -> bool *)
let rec ty_proc tyenv = function
  | Exp e ->
     let t, xs = ty_exp tyenv e in
     if un t then todo ()
                       (* lin なものが使わずに捨てられる *)
     else ty_err "violation of linearity: exp"
     (* un t *)
     (* xs = linear_vars(tyenv) になるのでは？ *)
     todo ()
  | Par (p,q) ->
     (* ここでも env splitting がある。 *)
     let _ = ty_proc tyenv p in
     let _ = ty_proc tyenv q in
     (* ty_proc でエラーにならなければ、型がつく *)
     todo ()
  | NuBind (c,d,s,p) ->
     let tyenv' = (E.add c (TySession s)
                     (E.add d (TySession (dual s)) tyenv)) in
     let _ = ty_proc tyenv' p in
     (* TODO: c,d は linear 変数なので使われないといけない *)
     todo ()
