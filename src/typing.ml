open Syntax
open Util

type tyenv = ty Environment.t

(* set of variables/ids *)
module IdSet =
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

exception Type_error of string
let ty_err s = raise (Type_error s)

exception Join_meet_undef

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
let un ty = mult_of_ty ty = Un
(* let un ty = not (lin ty) *)

(*** subtyping ***)

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
     let labels1 = LabelSet.of_list (List.map fst choices1) in
     let labels2 = LabelSet.of_list (List.map fst choices2) in
     if LabelSet.subset labels2 labels1 then
       (* all elements are in consistent subtyping *)
       List.for_all (fun l ->
           (* assoc will not fail *)
           let s = List.assoc l choices1 in
           let r = List.assoc l choices2 in
           con_sub_session s r)
         (LabelSet.elements labels2) (* J *)
     else false
  | TyCase choices1, TyCase choices2 ->
     let labels1 = LabelSet.of_list (List.map fst choices1) in
     let labels2 = LabelSet.of_list (List.map fst choices2) in
     if LabelSet.subset labels1 labels2 then
       List.for_all (fun l ->
           let s = List.assoc l choices1 in
           let r = List.assoc l choices2 in
           con_sub_session s r)
         (LabelSet.elements labels1) (* I *)
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

let join_mult (m : mult) (n : mult) : mult = match m,n with
  | Un, Un -> Un
  | Un, Lin -> Lin
  | Lin, Un -> Lin
  | Lin, Lin -> Lin

let meet_mult (m : mult) (n : mult) : mult = match m,n with
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
  | _ -> raise Join_meet_undef

and join_session (s : session) (r : session) : session = match s,r with
  | TySend (t1,s1), TySend (t2,s2) ->
     TySend (meet_ty t1 t2, join_session s1 s2)
  | TyReceive (t1,s1), TyReceive (t2,s2) ->
     TyReceive (join_ty t1 t2, join_session s1 s2)
  | TySelect choices1, TySelect choices2 ->
     (* intersection *)
     let labels1 = LabelSet.of_list (List.map fst choices1) in
     let labels2 = LabelSet.of_list (List.map fst choices2) in
     let labels3 = LabelSet.inter labels1 labels2 in
     let new_choices : (label * session) list =
       List.map (fun l ->
           let s = List.assoc l choices1 in
           let r = List.assoc l choices2 in
           try (l, join_session s r) with
           | Join_meet_undef -> raise Join_meet_undef)
         (LabelSet.elements labels3) in
     (* choice set cannot be empty in TySelect *)
     begin match new_choices with
     | [] -> raise Join_meet_undef
     | _ :: _ -> TySelect new_choices
     end

  | TyCase choices1, TyCase choices2 ->
     (* union *)
     let labels1 = LabelSet.of_list (List.map fst choices1) in
     let labels2 = LabelSet.of_list (List.map fst choices2) in
     (* I - J *)
     let labels3 = LabelSet.diff labels1 labels2 in
     let new_choices1 =
       List.map (fun l -> (l, List.assoc l choices1))
         (LabelSet.elements labels3) in
     (* I /\ J *)
     (* intersection, but ignore where join doesn't exist *)
     let labels4 = LabelSet.inter labels1 labels2 in
     let new_choice2_opt : (label * session) option list =
       List.map (fun l ->
           let s = List.assoc l choices1 in
           let r = List.assoc l choices2 in
           try Some (l, join_session s r) with
           | Join_meet_undef -> None)
         (LabelSet.elements labels4) in
     let new_choices2 = remove_none new_choice2_opt in
     (* J - I *)
     let labels5 = LabelSet.diff labels2 labels1 in
     let new_choices3 =
       List.map (fun l -> (l, List.assoc l choices2))
         (LabelSet.elements labels5) in
     (* Merge three choice lists; could be empty *)
     begin match new_choices1 @ new_choices2 @ new_choices3 with
     | [] -> raise Join_meet_undef
     | _ :: _ as choices' -> TyCase choices'
     end

  | TyClose, TyClose -> TyClose
  | TyWait, TyWait -> TyWait
  | TyDC, s -> s
  | s, TyDC -> s
  | _ -> raise Join_meet_undef

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
  | _ -> raise Join_meet_undef

and meet_session (s : session) (r : session) : session = match s,r with
  | TySend (t1,s1), TySend (t2,s2) ->
     TySend (join_ty t1 t2, meet_session s1 s2)
  | TyReceive (t1,s1), TyReceive (t2,s2) ->
     TyReceive (meet_ty t1 t2, meet_session s1 s2)
  | TySelect choices1, TySelect choices2 ->
     (* union *)
     let labels1 = LabelSet.of_list (List.map fst choices1) in
     let labels2 = LabelSet.of_list (List.map fst choices2) in
     (* I - J *)
     let labels3 = LabelSet.diff labels1 labels2 in
     let new_choices1 =
       List.map (fun l -> (l, List.assoc l choices1))
         (LabelSet.elements labels3) in
     (* I /\ J *)
     let labels4 = LabelSet.inter labels1 labels2 in
     let new_choices2_opt =
       List.map (fun l ->
           let s = List.assoc l choices1 in
           let r = List.assoc l choices2 in
           try Some (l, join_session s r) with
           | Join_meet_undef -> None)
         (LabelSet.elements labels4) in
     let new_choices2 = remove_none new_choices2_opt in
     (* J - I *)
     let labels5 = LabelSet.diff labels2 labels1 in
     let new_choices3 =
       List.map (fun l -> (l, List.assoc l choices2))
         (LabelSet.elements labels5) in
     (* Merge three choice lists; could be empty *)
     begin match new_choices1 @ new_choices2 @ new_choices3 with
     | [] -> raise Join_meet_undef
     | _ :: _ as choices' -> TySelect choices'
     end

  | TyCase choices1, TyCase choices2 ->
     (* intersection *)
     let labels1 = LabelSet.of_list (List.map fst choices1) in
     let labels2 = LabelSet.of_list (List.map fst choices2) in
     let labels3 = LabelSet.inter labels1 labels2 in
     let new_choices =
       List.map (fun l ->
           let s = List.assoc l choices1 in
           let r = List.assoc l choices2 in
           try (l, meet_session s r) with
           | Join_meet_undef -> raise Join_meet_undef)
         (LabelSet.elements labels3) in
     (* choice set cannot be empty in TyCase *)
     begin match new_choices with
     | [] -> raise Join_meet_undef
     | _ :: _ -> TyCase new_choices
     end

  | TyClose, TyClose -> TyClose
  | TyWait, TyWait -> TyWait
  | TyDC, s -> s
  | s, TyDC -> s
  | _ -> raise Join_meet_undef

let rec bigjoin (tys: ty list) : ty =
  (* TyDyn may work? then it would be better than hd *)
  List.fold_left (fun t_acc t ->
      join_ty t_acc t)
    (List.hd tys) tys


(*** Matching ***)

(* matching doesn't return type itself,
 * but arguments of the type constructor. *)

let matching_fun : ty -> mult * ty * ty = function
  | TyFun (m,t,u) -> (m,t,u)
  | TyDyn -> (Lin,TyDyn,TyDyn)
  | _ -> ty_err "MatchingFun"
  (* | t -> ty_err ("MatchingFun: failure " ^ (string_of_ty t)) *)

let matching_prod : ty -> mult * ty * ty  = function
  | TyProd (m,t,u) -> (m,t,u)
  | TyDyn -> (Lin,TyDyn,TyDyn)
  | _ -> ty_err "MatchingProd"

let matching_send : ty -> ty * session = function
  | TySession (TySend (t,s)) -> (t,s)
  | (TySession TyDC) | TyDyn -> (TyDyn, TyDC)
  | _ -> ty_err "MatchingSend"

let matching_receive : ty -> ty * session = function
  | TySession (TyReceive (t,s)) -> (t,s)
  | (TySession TyDC) | TyDyn -> (TyDyn, TyDC)
  | _ -> ty_err "MatchingReceive"

(* Check `if l is in choices' inside matching function *)
(* label in return value is not necessary *)
let matching_select (t : ty) (l : label) : (label * session) =
  match t with
  | TySession (TySelect choices) -> begin
      try
        let s = List.assoc l choices in (l, s)
      with
      | Not_found -> ty_err ("MatchingSelect: Label is not in choices: " ^ l)
    end
  (* (+){l : TyDC } *)
  | (TySession TyDC) | TyDyn -> (l, TyDC)
  | _ -> ty_err "MatchingSelect"

(* the order of labels are preserved from given ls *)
let matching_case (t : ty) (ls : label list) : (label * session) list =
  match t with
  | TySession (TyCase choices) ->
     let labelsI = LabelSet.of_list (List.map fst choices) in (* I *)
     let labelsJ = LabelSet.of_list ls in                     (* J *)
     if LabelSet.subset labelsI labelsJ then
       (* use choices for I; use DC for J - I *)
       List.map (fun l ->
           try
             let s = List.assoc l choices in (l, s)
           with
           | Not_found -> (l, TyDC)) ls
     (* I - J is missing branches in case *)
     else ty_err "MatchingCase: some branches are missing"
  | (TySession TyDC) | TyDyn ->
     List.map (fun l -> (l, TyDC)) ls
  | _ -> ty_err "MatchingCase"

(*** type checking ***)

let assert_disjoint (xs : IdSet.t) (ys : IdSet.t) : unit =
  let zs = IdSet.inter xs ys in
  (* display duplicate elements: zs? *)
  (* violation of linearity *)
  if IdSet.is_empty zs then ()
  else ty_err "Not disjoint sets"

(* type of binOp *)
let ty_binop : binOp -> ty * ty * ty = function
  | Plus | Minus | Mult | Div -> (TyInt, TyInt, TyInt)
  | (Lt | Gt | Eq | LE | GE) -> (TyInt, TyInt, TyBool)

(* returns the set of free linear variables used in typing *)
let rec ty_exp (tyenv : tyenv) (e : exp) : ty * IdSet.t =
  match e with
  | Var x -> begin
      try
        let t = Environment.find x tyenv in
        if lin t then (t, IdSet.singleton x)
        else (t, IdSet.empty)
      with
      | Not_found -> ty_err ("T-Var: Not bound: " ^ x)
    end
  | ULit -> (TyUnit, IdSet.empty)
  | ILit _ -> (TyInt, IdSet.empty)
  | BLit _ -> (TyBool, IdSet.empty)

  | BinOp (op, e1, e2) ->
     let t1, xs = ty_exp tyenv e1 in
     let t2, ys = ty_exp tyenv e2 in
     assert_disjoint xs ys;
     let u1, u2, u3 = ty_binop op in
     if con_sub_ty t1 u1 then
       if con_sub_ty t2 u2 then
         (u3, IdSet.union xs ys)
       else ty_err "T-BinOp-R"
     else ty_err "T-BinOp-L"

  | IfExp (e1,e2,e3) ->
     let t1, xs = ty_exp tyenv e1 in
     if con_ty t1 TyBool then
       let t2, ys = ty_exp tyenv e2 in
       let t3, zs = ty_exp tyenv e3 in
       if IdSet.equal ys zs then begin
           assert_disjoint xs ys; (* ys = zs *)
           (join_ty t2 t3, IdSet.union (IdSet.union xs ys) zs)
         end
       else ty_err "T-If: Same linear variables should be used in then/else clauses"
     else ty_err "T-If-Test: Not consistent with bool"

  | FunExp (m,x,t1,e1) ->
     let t2, ys = ty_exp (Environment.add x t1 tyenv) e1 in
     if lin t1 && m = Un then
       (* Y = {x} *)
       if IdSet.mem x ys then
         if IdSet.is_empty (IdSet.remove x ys) then
           (TyFun (Un, t1, t2), IdSet.empty)
         else ty_err "T-Fun: Unresterected function contains linear variables"
       else ty_err ("T-Fun: Unused linear variable: " ^ x)
     else if lin t1 && m = Lin then
       if IdSet.mem x ys then
         (TyFun (Lin, t1, t2), IdSet.remove x ys)
       else ty_err ("T-Fun: Unused linear variable: " ^ x)
     else if un t1 && m = Un then
       (* Y = {} *)
       if IdSet.is_empty ys then
         (TyFun (Un, t1, t2), IdSet.empty)
       else ty_err "T-Fun: Unresterected function contains linear variables"
            (* "unrestricted functions cannot contain variables of a linear type" *)
     else (TyFun (Lin, t1, t2), ys) (* un t1 && m = Lin *)

  | FixExp (m,x,y,t1,t2,e1) ->
     (* NOTE: if x is linear (m = Lin),
      * x should be used once in e1 *)

     let tyenv1 = Environment.add x (TyFun (m,t1,t2)) tyenv in
     let tyenv2 = Environment.add y t1 tyenv1 in
     let t2', zs = ty_exp tyenv2 e1 in
     (* consistency rather than equality *)
     if not (con_ty t2' t2) then
       ty_err "T-Fix: return type does not equal the given annotation";
     if lin t2 && m = Un then
       (* Z = {y} ; y should be used *)
       if IdSet.mem y zs then
         if IdSet.is_empty (IdSet.remove y zs) then
           (TyFun (Un,t1,t2), IdSet.empty)
         else ty_err "T-Fix: Unresterected function contains linear variables"
       else ty_err ("T-Fix: Unused linear variable: " ^ y)
     else if lin t2 && m = Lin then
       (* x,y should be used *)
       if IdSet.mem x zs then
         if IdSet.mem y zs then
           (TyFun (Lin,t1,t2),
            IdSet.remove x (IdSet.remove y zs))
         else ty_err ("T-Fix: Unused linear variable: " ^ y)
       else ty_err ("T-Fix: Unused linear variable: " ^ x)
     else if un t2 && m = Un then
       (* Z = {} *)
       if IdSet.is_empty zs then
         (TyFun (Un,t1,t2), IdSet.empty)
       else ty_err "T-Fix: Unresterected function contains linear variables"
     else (* un t2, m = Lin *)
       (* x should be used *)
       if IdSet.mem x zs then
         (TyFun (Lin,t1,t2), IdSet.remove x zs)
       else ty_err ("T-Fix: Unused linear variable: " ^ x)

  | AppExp (e1,e2) ->
     let t1, xs = ty_exp tyenv e1 in
     let t2, ys = ty_exp tyenv e2 in
     assert_disjoint xs ys;
     let _, t11, t12 = matching_fun t1 in
     if con_sub_ty t2 t11 then (t12, IdSet.union xs ys)
     else ty_err "T-App: Not consistent subtype"

  | LetExp (x,e1,e2) ->
     let t1, xs = ty_exp tyenv e1 in
     let t2, ys = ty_exp (Environment.add x t1 tyenv) e2 in
     assert_disjoint xs ys;
     (* if lin(t1), x should be used in e2 *)
     if lin t1 then
       if IdSet.mem x ys then
         (t2, IdSet.union xs (IdSet.remove x ys))
       else ty_err ("T-Let: Unused linear variable: " ^ x)
     else (t2, IdSet.union xs ys) (* un(t) *)

  | PairCons (m,e1,e2) ->
     let t1, xs = ty_exp tyenv e1 in
     let t2, ys = ty_exp tyenv e2 in
     assert_disjoint xs ys;     (* Can I put this here? maybe below? *)
     if m = Un then
       (* NOTE: xs, ys need not be empty *)
       if un t1 && un t2 then
         (TyProd (Un,t1,t2), IdSet.union xs ys)
       else ty_err "T-PairCons: Linear element in unrestrected pair"
     else (TyProd (Lin,t1,t2), IdSet.union xs ys)

  | PairDest (x1,x2,e1,e2) ->
     let t, ys = ty_exp tyenv e1 in
     let _, t1, t2 = matching_prod t in
     let u, zs = ty_exp (Environment.add x1 t1
                           (Environment.add x2 t2 tyenv)) e2
     in
     (* TODO: assert_disjoint here? *)
     if lin t1 && lin t2 then
       if IdSet.mem x1 zs then
         if IdSet.mem x2 zs then
           let zs' = IdSet.remove x1 (IdSet.remove x2 zs) in
           assert_disjoint ys zs';
           (u, IdSet.union ys zs')
         else ty_err ("T-PairDest: Unused linear variable: " ^ x2)
       else ty_err ("T-PairDest: Unused linear variable: " ^ x1)
     else if lin t1 && un t2 then
       if IdSet.mem x1 zs then
         let zs' = IdSet.remove x1 zs in
         assert_disjoint ys zs';
         (u, IdSet.union ys zs')
       else ty_err ("T-PairDest: Unused linear variable: " ^ x1)
     else if un t1 && lin t2 then
       if IdSet.mem x2 zs then
         let zs' = IdSet.remove x2 zs in
         assert_disjoint ys zs';
         (u, IdSet.union ys zs')
       else ty_err ("T-PairDest: Unused linear variable: " ^ x2)
     else let _ = assert_disjoint ys zs in
          (u, IdSet.union ys zs)

  | ForkExp e ->
     let t, xs = ty_exp tyenv e in
     if con_ty t TyUnit then (TyUnit, xs)
     else ty_err "T-Fork: Not consistent with unit"

  | NewExp s ->
     let t = TyProd (Lin, TySession s, TySession (dual s)) in
     (t, IdSet.empty)

  | SendExp (e1,e2) ->          (* e2 is channel *)
     let t1, xs = ty_exp tyenv e1 in
     let t2, ys = ty_exp tyenv e2 in
     assert_disjoint xs ys;
     let t3, s = matching_send t2 in
     if con_sub_ty t1 t3
     then (TySession s, IdSet.union xs ys)
     else ty_err "T-Send: Not consistent subtype"

  | ReceiveExp e1 ->
     let t1, xs = ty_exp tyenv e1 in
     let t2, s = matching_receive t1 in
     (TyProd (Lin, t2, TySession s), xs)

  | SelectExp (l,e) ->
     let t, xs = ty_exp tyenv e in
     let (_, s) = matching_select t l in
     (TySession s, xs)

  | CaseExp (e0, branches) ->
     let t, xs = ty_exp tyenv e0 in
     let ls = List.map (fun (l,_,_,_) -> l) branches in (* = J *)
     let choices = matching_case t ls in

     (* assume: labels in choices/branches appear in same order *)
     let (us : ty list), (yss : IdSet.t list) =
       List.split
         (List.map (fun ((l,x,s1_opt,e), (l', s2)) ->
              assert (l = l');
              (* s1 is the given annotation *)
              let s = match s1_opt with
                | Some s1 -> if con_sub_session s2 s1 then s1
                             else ty_err "T-Case: inconsistent type annotation"
                | None -> s2
              in
              let u, ys = ty_exp (Environment.add x (TySession s) tyenv) e in
              if IdSet.mem x ys then
                (u, IdSet.remove x ys) (* Uj,Yj *)
              else ty_err ("T-Case: Unused linear variable: " ^ x))
            (List.combine branches choices))
     in
     (* ys' = ys1 = ys2 = ... *)
     let ys' : IdSet.t =
       List.fold_left (fun ys_acc ys ->
           if IdSet.equal ys ys_acc then ys
           else ty_err ("T-Case: Same linear variables should be used in case branches"))
         (List.hd yss)        (* use first elem *)
         yss
     in
     begin
       try
         let u' = bigjoin us in
         assert_disjoint xs ys';
         (u', IdSet.union xs ys')
       with
       | Join_meet_undef -> ty_err "T-Case: Join undefined"
     end

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
             if un t && IdSet.is_empty xs then begin
               print_string "The program is well-typed.\n";
               Printf.printf "Type of expression: %s\n" (string_of_ty t)
               end
             else ty_err "T-Exp: ill-typed program"
