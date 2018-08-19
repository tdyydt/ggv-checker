open Syntax
open Typing

(* not eval, but type check *)
let rec read_eval_print tyenv =
  print_string "# ";
  flush stdout;
  let lexbuf = Lexing.from_channel stdin in
  try
    let exp = Parser.toplevel Lexer.main lexbuf in
    let ty, _ = ty_exp tyenv exp in
    (* Printf.printf "" *)
    print_string (string_of_ty ty);
    print_newline ();
    read_eval_print tyenv
  with
  (* parse error (Menhir),
   * lexer error,
   * or typing error etc. *)
  | Typing_error s -> print_string s;
                      print_newline ();
                      read_eval_print tyenv


(* empty environment *)
let _ = read_eval_print Environment.empty
