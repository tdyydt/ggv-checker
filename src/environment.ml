module M =
  Map.Make (
      struct
        type t = Syntax.id
        let compare (x : Syntax.id) y = compare x y
      end
    )

type 'a t = 'a M.t


let empty = M.empty

let add x v env =
  if M.mem x env then
    (* variable shadowing is not supported/allowed *)
    failwith ("currently you cannot use same names twice: " ^ x)
  else M.add x v env

let remove x env = M.remove x env

let find x env = M.find x env

let mem = M.mem

(* keys, values *)
(* dom, codom(range) *)

(* pick up values from a environment *)
(* cod(env) *)
let values env =
  M.fold (fun k v vs -> v :: vs) env []
