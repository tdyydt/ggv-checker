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

(* choice labels *)
module LabelSet =
  Set.Make(
      struct
        type t = label
        let compare (x : label) y = compare x y
      end
    )

exception Typing_error of string
let ty_err s = raise (Typing_error s)

(*** Types ***)

(* duality *)
let rec dual : session -> session = function
  | TySend (t,s) -> TyReceive (t, dual s)
  | TyReceive (t,s) -> TySend (t, dual s)
  | TySelect choices ->
     TyCase (List.map (fun (l,s) -> (l, dual s)) choices)
  | TyCase choices ->
     TySelect (List.map (fun (l,s) -> (l, dual s)) choices)
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
  | _ -> false               (* t,u has different type constructors *)

(* [s <~ r] *)
and con_sub_session (s : session) (r : session) : bool =
  match s,r with
  | TySend (t1,s1), TySend (t2,s2) ->
     con_sub_ty t2 t1
     && con_sub_session s1 s2
  | TyReceive (t1,s1), TyReceive (t2,s2) ->
     con_sub_ty t1 t2
     && con_sub_session s1 s2
  | TySelect choices1, TySelect choices2 ->
     let labels1 = List.map (fun (l,_) -> l) choices1 in (* fst *)
     let labels2 = List.map (fun (l,_) -> l) choices2 in
     if LabelSet.subset (LabelSet.of_list labels2)
          (LabelSet.of_list labels1) then
       (* all elements are in consistent subtyping *)
       List.for_all (fun l ->
           (* assoc will not fail *)
           let s = List.assoc l choices1 in
           let r = List.assoc l choices2 in
           con_sub_session s r)
         labels2             (* J *)
     else false
  | TyCase choices1, TyCase choices2 ->
     let labels1 = List.map fst choices1 in
     let labels2 = List.map fst choices2 in
     if LabelSet.subset (LabelSet.of_list labels1)
          (LabelSet.of_list labels2) then
       List.for_all (fun l ->
           let s = List.assoc l choices1 in
           let r = List.assoc l choices2 in
           con_sub_session s r)
         labels1             (* I *)
     else false
  | TyClose, TyClose -> true
  | TyWait, TyWait -> true
  | TyDC, _ -> true
  | _, TyDC -> true
  | _ -> false

(* consistency: [t ~ u] *)
let rec con_ty (t : ty) (u : ty) : bool = match t,u with
  | TyUnit, TyUnit -> true
  | TyInt, TyInt -> true
  | TyBool, TyBool -> true
  | TySession s, TySession r -> con_session s r
  | TyFun (m,t1,u1), TyFun (n,t2,u2) when m = n ->
     con_ty t2 t1 && con_ty u1 u2
  | TyProd (m,t1,u1), TyProd (n,t2,u2) when m = n ->
     con_ty t1 t2 && con_ty u1 u2
  | TyDyn, _ -> true
  | _, TyDyn -> true
  | _ -> false               (* t,u has different type constructors *)

(* [s ~ r] *)
and con_session (s : session) (r : session) : bool =
  match s,r with
  | TySend (t1,s1), TySend (t2,s2) ->
     con_ty t2 t1 && con_session s1 s2
  | TyReceive (t1,s1), TyReceive (t2,s2) ->
     con_ty t1 t2 && con_session s1 s2

  | TySelect choices1, TySelect choices2 ->
     let labels1 = LabelSet.of_list (List.map fst choices1) in
     let labels2 = LabelSet.of_list (List.map fst choices2) in
     if LabelSet.equal labels1 labels2 then
       (* all pairs are in consistency *)
       List.for_all (fun l ->
           (* assoc will not fail *)
           let s = List.assoc l choices1 in
           let r = List.assoc l choices2 in
           con_session s r)
         (LabelSet.elements labels1) (* I = J *)
     else false
  | TyCase choices1, TyCase choices2 ->
     let labels1 = LabelSet.of_list (List.map fst choices1) in
     let labels2 = LabelSet.of_list (List.map fst choices2) in
     if LabelSet.equal labels1 labels2 then
       List.for_all (fun l ->
           let s = List.assoc l choices1 in
           let r = List.assoc l choices2 in
           con_session s r)
         (LabelSet.elements labels1) (* I = J *)
     else false
  | TyClose, TyClose -> true
  | TyWait, TyWait -> true
  | TyDC, _ -> true
  | _, TyDC -> true
  | _ -> false

(*** join/meet ***)

let join_mult (m : mult) (n : mult) :mult = match m,n with
  | Un, Un -> Un
  | Un, Lin -> Lin
  | Lin, Un -> Lin
  | Lin, Lin -> Lin

let meet_mult (m : mult) (n : mult) :mult = match m,n with
  | Un, Un -> Un
  | Un, Lin -> Un
  | Lin, Un -> Un
  | Lin, Lin -> Lin

(* lifted join/meet (subtyping & precision) *)
let rec join_ty (t : ty) (u : ty) : ty = match t,u with
  (* | t, u when t = u -> t *)
  | TyUnit, TyUnit -> TyUnit
  | TyFun (m,t1,u1), TyFun (n,t2,u2) ->
     TyFun (join_mult m n,
            meet_ty t1 t2,
            join_ty u1 u2)
  | TyProd (m, t1, u1), TyProd (n, t2, u2) ->
     TyProd (join_mult m n,
             join_ty t1 t2,
             join_ty u1 u2)
  | TySession s, TySession r -> TySession (join_session s r)
  | TyDyn, t -> t
  | t, TyDyn -> t
  | _ -> ty_err "join_ty: undefined"

and join_session (s : session) (r : session) : session = match s,r with
  | TySend (t1,s1), TySend (t2,s2) ->
     TySend (meet_ty t1 t2, join_session s1 s2)
  | TyReceive (t1,s1), TyReceive (t2,s2) ->
     TyReceive (join_ty t1 t2, join_session s1 s2)
  | TySelect choices1, TySelect choices2 ->
     let labels1 = LabelSet.of_list (List.map fst choices1) in
     let labels2 = LabelSet.of_list (List.map fst choices2) in
     (* I - J *)
     let labels3 = LabelSet.diff labels1 labels2 in
     let new_choices1 =
       List.map (fun l -> (l, List.assoc l choices1))
         (LabelSet.elements labels3) in
     (* I /\ J *)
     let labels4 = LabelSet.inter labels1 labels2 in
     let new_choices2 =         (* empty is ok *)
       List.map (fun l ->
           let s = List.assoc l choices1 in
           let r = List.assoc l choices2 in
           (l, join_session s r))
         (LabelSet.elements labels4) in
     (* J - I *)
     let labels5 = LabelSet.diff labels2 labels1 in
     let new_choices3 =
       List.map (fun l -> (l, List.assoc l choices2))
         (LabelSet.elements labels5) in
     (* Merge three choice lists; cannot be empty *)
     TySelect (new_choices1 @ new_choices2 @ new_choices3)

  | TyCase choices1, TyCase choices2 ->
     let labels1 = List.map fst choices1 in
     let labels2 = List.map fst choices2 in
     (* intersection of lists *)
     let labels3 = LabelSet.inter (LabelSet.of_list labels1)
                     (LabelSet.of_list labels2) in
     (* choice set cannot be empty in TyCase *)
     if LabelSet.is_empty labels3 then
       ty_err "join_session: undefined"
     else
       TyCase (List.map (fun l ->
                   let s = List.assoc l choices1 in
                   let r = List.assoc l choices2 in
                   (l, join_session s r))
                 (LabelSet.elements labels3))

  | TyClose, TyClose -> TyClose
  | TyWait, TyWait -> TyWait
  | TyDC, s -> s
  | s, TyDC -> s
  | _ -> ty_err "join_session: undefined"

and meet_ty (t : ty) (u : ty) : ty = match t,u with
  | TyUnit, TyUnit -> TyUnit
  | TyFun (m,t1,u1), TyFun (n,t2,u2) ->
     TyFun (meet_mult m n,
            join_ty t1 t2,
            meet_ty u1 u2)
  | TyProd (m, t1, u1), TyProd (n, t2, u2) ->
     TyProd (meet_mult m n,
             meet_ty t1 t2,
             meet_ty u1 u2)
  | TySession s, TySession r -> TySession (meet_session s r)
  | TyDyn, t -> t
  | t, TyDyn -> t
  | _ -> ty_err "meet_ty: undefined"

and meet_session (s : session) (r : session) : session = match s,r with
  | TySend (t1,s1), TySend (t2,s2) ->
     TySend (join_ty t1 t2, meet_session s1 s2)
  | TyReceive (t1,s1), TyReceive (t2,s2) ->
     TyReceive (meet_ty t1 t2, meet_session s1 s2)
  | TySelect choices1, TySelect choices2 ->
     let labels1 = List.map fst choices1 in
     let labels2 = List.map fst choices2 in
     let labels3 = LabelSet.inter (LabelSet.of_list labels1)
                     (LabelSet.of_list labels2) in
     if LabelSet.is_empty labels3 then
       ty_err "meet_session: undefined"
     else
       TySelect (List.map (fun l ->
                     let s = List.assoc l choices1 in
                     let r = List.assoc l choices2 in
                     (l, meet_session s r))
                   (LabelSet.elements labels3))

  | TyCase choices1, TyCase choices2 ->
     let labels1 = LabelSet.of_list (List.map fst choices1) in
     let labels2 = LabelSet.of_list (List.map fst choices2) in
     (* I - J *)
     let labels3 = LabelSet.diff labels1 labels2 in
     let new_choices1 =
       List.map (fun l -> (l, List.assoc l choices1))
         (LabelSet.elements labels3) in
     (* I /\ J *)
     let labels4 = LabelSet.inter labels1 labels2 in
     let new_choices2 =         (* empty is ok *)
       List.map (fun l ->
           let s = List.assoc l choices1 in
           let r = List.assoc l choices2 in
           (l, meet_session s r))
         (LabelSet.elements labels4) in
     (* J - I *)
     let labels5 = LabelSet.diff labels2 labels1 in
     let new_choices3 =
       List.map (fun l -> (l, List.assoc l choices2))
         (LabelSet.elements labels5) in
     (* Merge three choice lists; cannot be empty *)
     TyCase (new_choices1 @ new_choices2 @ new_choices3)

  | TyClose, TyClose -> TyClose
  | TyWait, TyWait -> TyWait
  | TyDC, s -> s
  | s, TyDC -> s
  | _ -> ty_err "meet_session: undefined"

let rec bigjoin (tys: ty list) : ty =
  (* TyDyn may work? and it is better than hd *)
  List.fold_left (fun t_acc t ->
      join_ty t_acc t)
    (List.hd tys) tys


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

(*** type checking ***)

let assert_disjoint (xs : VarSet.t) (ys : VarSet.t) =
  let zs = VarSet.inter xs ys in
  (* display duplicate elements: zs? *)
  (* violation of linearity *)
  if VarSet.is_empty zs then ()
  else ty_err "Not disjoint sets"

(* VarSet.t -> VarSet.t -> bool *)
(* let is_disjoint xs ys =
 *   VarSet.is_empty (VarSet.inter xs ys) *)

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
     (* if lin(t1), x should be used in e2 *)
     if lin t1 then
       if VarSet.mem x ys then
         (t2, VarSet.union xs (VarSet.remove x ys))
       else ty_err ("T-Let: Unused linear variable: " ^ x)
     else (t2, VarSet.union xs ys) (* un(t) *)

  | PairCons (m,e1,e2) ->
     let t1, xs = ty_exp tyenv e1 in
     let t2, ys = ty_exp tyenv e2 in
     assert_disjoint xs ys;     (* Can I put this here? maybe below? *)
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
     if con_ty t TyUnit then (TyUnit, xs)
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
     else ty_err "T-Send: Not consistent subtype"

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

  | CaseExp (e0, branches) ->
     let t, xs = ty_exp tyenv e0 in
     (* choices of branches *)
     let choices = List.map (fun (l,_,s,_) -> (l,s)) branches in
     if con_sub_ty t (TySession (TyCase choices)) then
       let (us : ty list), (yss : VarSet.t list) =
         List.split
           (List.map (fun (l,x,s,e) ->
                let u, ys = ty_exp (Environment.add x (TySession s) tyenv) e in
                if VarSet.mem x ys then
                  (u, VarSet.remove x ys) (* Uj,Yj *)
                else ty_err ("T-Case: Unused linear variable: " ^ x))
              branches)
       in
       (* ys' = ys1 = ys2 = ... *)
       let ys' : VarSet.t =
         List.fold_left (fun ys_acc ys ->
             if VarSet.equal ys ys_acc then ys
             else ty_err ("T-Case: Same linear variables should be used in case branches"))
           (List.hd yss)        (* use first elem *)
           yss
       in
       let u' = bigjoin us in
       assert_disjoint xs ys';
       (u', VarSet.union xs ys')
     else ty_err "T-Case: Not consistent subtype"

  | CloseExp e ->
     let t, xs = ty_exp tyenv e in
     if con_ty t (TySession TyClose) then (TyUnit, xs)
     else ty_err "T-Close: not consistent with end! (close)"

  | WaitExp e ->
     let t, xs = ty_exp tyenv e in
     if con_ty t (TySession TyWait) then (TyUnit, xs)
     else ty_err "T-Wait: not consistent with end? (wait)"


let ty_prog : prog -> unit = function
  | Exp e -> let t, xs = ty_exp Environment.empty e in
             if un t && VarSet.is_empty xs then
               print_string "The program is well-typed.\n"
             else ty_err "T-Exp: ill-typed program"
