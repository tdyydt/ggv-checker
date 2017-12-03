{
module P = Parser

let reservedWords = [
    ("lin", P.LIN);
    ("un", P.UN);

    ("unit", P.UNIT);
    ("int", P.INT);
    ("bool", P.BOOL);
    ("end", P.END);

    ("true", P.TRUE);
    ("false", P.FALSE);

    ("fun", P.FUN);
    ("let", P.LET);
    ("in", P.IN);

    ("fork", P.FORK);
    ("new", P.NEW);
    ("send", P.SEND);
    ("receive", P.RECEIVE);
    ("select", P.SELECT);
    ("case", P.CASE);
    ("close", P.CLOSE);
    ("wait", P.WAIT);

    ("nu", P.NU);
  ]

}

rule main = parse
(* ignore whitespace characters *)
| [' ' '\009' '\012' '\n']+     { main lexbuf }

(* integer literal *)
| "-"? ['0'-'9']+
    { P.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| ";;" { P.SEMISEMI }

| "(" { P.LPAREN }
| ")" { P.RPAREN }
| "{" { P.LBRACE }
| "}" { P.RBRACE }

| "->" { P.RARROW }
| "!" { P.PL }
| "?" { P.QU }
| "&" { P.AMP }
| ":" { P.COLON }
| "." { P.PERIOD }
| "," { P.COMMA }

| "+" { P.PLUS }
| "*" { P.STAR }
| "#" { P.HASH }

| "=" { P.EQ }
| "<" { P.LT }
(* | ">" { P.GT } *)

| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try
        List.assoc id reservedWords
      with
      | _ -> Parser.ID id
     }
