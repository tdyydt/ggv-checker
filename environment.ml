(* これで、本当に出来ているのか？
 * TODO: mli を作る *)

module M =
  Map.Make (
      struct
        type t = Syntax.id
        let compare = compare
      end
    )

type 'a t = 'a M.t


let empty = M.empty

let add x v env = M.add x v env

let remove x env = M.remove x env

let find x env = M.find x env

let mem = M.mem

(* keys, values *)
(* dom, codom(range) *)

(* pick up values from a environment *)
(* cod(env) *)
let values env =
  M.fold (fun k v vs -> v :: vs) env []
