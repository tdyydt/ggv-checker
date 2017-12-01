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

(* env の値だけ取る
 * 型環境の型だけ *)
let values = ()
