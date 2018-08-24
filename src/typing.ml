open Syntax

type tyenv = ty Environment.t

(* set of variable *)
(* OR: IdSet, Variables *)
module VarSet =
  Set.Make(
      struct
        type t = id
        let compare (x : id) y = compare x y
      end
    )

exception Typing_error of string
let ty_err s = raise (Typing_error s)

(*** Types ***)

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

(* join/meet wrt. subtyping *)
let rec join (s : session) (t : session) : session = todo ()
and meet (s : session) (t : session) : session = todo ()

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

(*** Matching ***)

(* matching doesn't return type itself,
 * but arguments of the type constructor. *)

let matching_fun : ty -> mult * ty * ty = function
  | TyFun (m,t,u) -> (m,t,u)
  | TyDyn -> (Lin,TyDyn,TyDyn)
  | _ -> ty_err "matching error: fun"

let matching_prod : ty -> mult * ty * ty  = function
  | TyProd (m,t,u) -> (m,t,u)
  | TyDyn -> (Lin,TyDyn,TyDyn)
  | _ -> ty_err "matching error: prod"

let matching_send : ty -> ty * session = function
  | TySession (TySend (t,s)) -> (t,s)
  | (TySession TyDC) | TyDyn -> (TyDyn, TyDC)
  | _ -> ty_err "matching error: send"

let matching_receive : ty -> ty * session = function
  | TySession (TyReceive (t,s)) -> (t,s)
  | (TySession TyDC) | TyDyn -> (TyDyn, TyDC)
  | _ -> ty_err "matching error: receive"

let matching_select (t : ty) (l : label) : (label * session) list =
  match t with
  | TySession (TySelect br) -> br
  (* (+){l : TyDC } *)
  | (TySession TyDC) | TyDyn -> [(l, TyDC)]
  | _ -> ty_err "matching error: select"

let matching_case (t : ty) (ls : label list) : (label * session) list =
  match t with
  | TySession (TyCase br) -> br
  | (TySession TyDC) | TyDyn ->
     List.map (fun l -> (l, TyDC)) ls
  | _ -> ty_err "matching error: case"

(*** type checking ***)

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

let assert_disjoint (xs : VarSet.t) (ys : VarSet.t) =
  let zs = VarSet.inter xs ys in
  (* display duplicate elements: zs? *)
  (* violation of linearity *)
  if VarSet.is_empty zs then ()
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

(* type of binOp *)
let ty_binop : binOp -> ty * ty * ty = function
  | Plus | Minus | Mult | Div -> (TyInt, TyInt, TyInt)

(* returns the set of free linear variables used in typing *)
let rec ty_exp (tyenv : tyenv) (e : exp) : ty * VarSet.t =
  match e with
  | Var x -> begin
      try
        let t = Environment.find x tyenv in
        if lin t then (t, VarSet.singleton x)
        else (t, VarSet.empty)
      with
      | Not_found -> ty_err ("T-Var: Not bound: " ^ x)
    end
  | ULit -> (TyUnit, VarSet.empty)
  | ILit _ -> (TyInt, VarSet.empty)
  | BLit _ -> (TyBool, VarSet.empty)

  | BinOp (op, e1, e2) ->
     let t1, xs = ty_exp tyenv e1 in
     let t2, ys = ty_exp tyenv e2 in
     assert_disjoint xs ys;
     let u1, u2, u3 = ty_binop op in
     if con_sub_ty t1 u1 then
       if con_sub_ty t2 u2 then
         (u3, VarSet.union xs ys)
       else ty_err "T-BinOp-R"
     else ty_err "T-BinOp-L"

  | FunExp (m,x,t1,e1) ->
     assert (not (Environment.mem x tyenv)); (* FIXME *)

     let t2, ys = ty_exp (Environment.add x t1 tyenv) e1 in
     if lin t1 && m = Un then
       (* Y = {x} *)
       if VarSet.mem x ys then
         if VarSet.is_empty (VarSet.remove x ys) then
           (TyFun (Un, t1, t2), VarSet.empty)
         else ty_err "T-Fun: Unresterected function contains linear variables"
       else ty_err ("T-Fun: Unused linear variable: " ^ x)
     else if lin t1 && m = Lin then
       if VarSet.mem x ys then
         (TyFun (Lin, t1, t2), VarSet.remove x ys)
       else ty_err ("T-Fun: Unused linear variable: " ^ x)
     else if un t1 && m = Un then
       (* Y = {} *)
       if VarSet.is_empty ys then
         (TyFun (Un, t1, t2), VarSet.empty)
       else ty_err "T-Fun: Unresterected function contains linear variables"
            (* "unrestricted functions cannot contain variables of a linear type" *)
     else (TyFun (Lin, t1, t2), ys) (* un t1 && m = Lin *)

  | AppExp (e1,e2) ->
     let t1, xs = ty_exp tyenv e1 in
     let t2, ys = ty_exp tyenv e2 in
     assert_disjoint xs ys;
     let _, t11, t12 = matching_fun t1 in
     if con_sub_ty t2 t11 then (t12, VarSet.union xs ys)
     else ty_err "T-App: Not consistent subtype"

  | LetExp (x,e1,e2) ->
     assert (not (Environment.mem x tyenv)); (* FIXME *)

     let t1, xs = ty_exp tyenv e1 in
     let t2, ys = ty_exp (Environment.add x t1 tyenv) e2 in
     assert_disjoint xs ys;
     (* if lin(t), x should be used in e2 *)
     if lin t then
       if VarSet.mem x ys then
         (t2, VarSet.union xs (VarSet.remove x ys))
       else ty_err ("T-Let: Unused linear variable: " ^ x)
     else (t2, VarSet.union xs ys) (* un(t) *)

  | PairCons (m,e1,e2) ->
     let t1, xs = ty_exp tyenv e1 in
     let t2, ys = ty_exp tyenv e2 in
     assert_disjoint xs ys;     (* タイミングが変？ *)
     if m = Un then
       (* NOTE: xs, ys need not be empty *)
       if lin t1 && lin t2 then
         (TyProd (Un,t1,t2), VarSet.union xs ys)
       else ty_err "T-PairCons: Linear element in unrestrected pair"
     else (TyProd (Lin,t1,t2), VarSet.union xs ys)

  | PairDest (x1,x2,e1,e2) ->
     (* 変数名の上書きを(一時的に)禁止 *)
     assert (not (Environment.mem x1 tyenv));
     assert (not (Environment.mem x2 tyenv));

     let t, ys = ty_exp tyenv e1 in
     let _, t1, t2 = matching_prod t in
     let u, zs = ty_exp (Environment.add x1 t1
                           (Environment.add x2 t2 tyenv)) e2
     in
     (* TODO: assert_disjoint here? *)
     if lin t1 && lin t2 then
       if VarSet.mem x1 zs then
         if VarSet.mem x2 zs then
           let zs' = VarSet.remove x1 (VarSet.remove x2 zs) in
           assert_disjoint ys zs';
           (u, VarSet.union ys zs')
         else ty_err ("T-PairDest: Unused linear variable: " ^ x2)
       else ty_err ("T-PairDest: Unused linear variable: " ^ x1)
     else if lin t1 && un t2 then
       if VarSet.mem x1 zs then
         let zs' = VarSet.remove x1 zs in
         assert_disjoint ys zs';
         (u, VarSet.union ys zs')
       else ty_err ("T-PairDest: Unused linear variable: " ^ x1)
     else if un t1 && lin t2 then
       if VarSet.mem x2 zs then
         let zs' = VarSet.remove x2 zs in
         assert_disjoint ys zs';
         (u, VarSet.union ys zs')
       else ty_err ("T-PairDest: Unused linear variable: " ^ x2)
     else let _ = assert_disjoint ys zs in
          (u, VarSet.union ys zs)

  | ForkExp e ->
     let t, xs = ty_exp tyenv e in
     (* TODO: replace with con_ty? *)
     if con_sub_ty t TyUnit then (TyUnit, xs)
     else ty_err "T-Fork: Not consistent with unit"

  | NewExp s ->
     let t = TyProd (Lin, TySession s, TySession (dual s)) in
     (t, VarSet.empty)

  | SendExp (e1,e2) ->          (* e2 is channel *)
     let t1, xs = ty_exp tyenv e1 in
     let t2, ys = ty_exp tyenv e2 in
     assert_disjoint xs ys;
     let t3, s = matching_send t2 in
     if con_sub_ty t1 t3
     then (TySession s, VarSet.union xs ys)
     else ty_err "Not consistent subtype: send"

  | ReceiveExp e1 ->
     let t1, xs = ty_exp tyenv e1 in
     let t2, s = matching_receive t1 in
     (TyProd (Lin, t2, TySession s), xs)

  | SelectExp (l,e) ->
     let t, xs = ty_exp tyenv e in
     let choices = matching_select t l in
     begin
       try
         (* Check if l is in choices *)
         (* NOTE: This check can be done within matching_select *)
         let s = List.assoc l choices in
         (TySession s, xs)
       with
       | Not_found ->
          ty_err ("T-Select: Label is not in choices: " ^ l)
     end

  (* br=branch *)
  | CaseExp (e, brs) ->
     let t, xs = ty_exp tyenv e in
     (* pick up labels *)
     let ls = List.map (fun (l,_,_,_) -> l) brs in
     let ty_brs = matching_case t ls in
     begin
       try
         (* 式 f[i] の型 u[i],
          * その型付けで用いた linear 変数集合 ys[i]
          * の組たちのリスト *)
         let _ =
           List.map (fun (l,x,_,f) ->
               let s = List.assoc l ty_brs in
               let u, ys = ty_exp (Environment.add x (TySession s) tyenv) f in

               (* TODO: x は linear変数なので、使われないといけない *)

               (u, VarSet.remove x ys))
             brs in
         (* ys[i] たちは bigUnion を取る。
          * u[i] たちは等しいはず
          * assertEq か？ *)
         (* => そうではなくて、
          * 等しくなくても subtype 関係であればいいので、
          * もっとも super な型を取る操作をやればいい *)
         todo ()
       with
       (* どこかの部分式でエラーが起きた場合 *)
       | Not_found -> ty_err "label is not in branches"
     end


  | CloseExp e ->
     let t, xs = ty_exp tyenv e in
     (* TODO: con_ty? *)
     if con_sub_ty t (TySession TyClose) then (TyUnit, xs)
     else ty_err "T-Close: not consistent with end! (close)"

  | WaitExp e ->
     let t, xs = ty_exp tyenv e in
     (* TODO: con_ty? *)
     if con_sub_ty t (TySession TyWait) then (TyUnit, xs)
     else ty_err "T-Wait: not consistent with end? (wait)"


let ty_prog : prog -> unit = function
  | Exp e -> let t, xs = ty_exp Environment.empty e in
             if un t && VarSet.is_empty xs then
               print_string "The program is Well-typed.\n"
             else ty_err "T-Exp: ill-typed program"
