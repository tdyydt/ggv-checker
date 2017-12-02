open Syntax
open Typing

let rec read_eval_print tyenv =
  print_string "# ";
  flush stdout;
  try
    let proc = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    (* とりあえず Exp だけを想定 *)
    let Exp exp = proc in
    (* let () = ty_proc *)
    let ty, _ = ty_exp tyenv exp in
    (* Printf.printf "" *)
    print_string (string_of_ty ty);
    print_newline ()
  with
  (* parse error (Menhir),
   * lexer error,
   * or typing error etc. *)
  | _ -> print_string "error";
         print_newline ()


(* empty environment *)
let _ = read_eval_print Environment.empty
