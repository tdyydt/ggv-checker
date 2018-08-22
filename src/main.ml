open Syntax
open Typing
open Printf

(* type check prog *)
(* NOTE: tyenv is removed *)
let rec read_check_print () =
  print_string "# ";
  flush stdout;
  let lexbuf = Lexing.from_channel stdin in
  try
    let prog = Parser.toplevel Lexer.main lexbuf in
    ty_prog prog;
    read_check_print ()
  with
  (* parse error (Menhir),
   * lexer error,
   * or typing error etc. *)
  | Typing_error s -> print_string s;
                      print_newline ();
                      read_check_print ()

let _ = read_check_print ()
