{
module Parser = P

let reservedWords = [
    ("lin", P.LIN);
    ("un", P.UN);
    ("unit", P.UNIT);

    ("fun", P.FUN);
    ("let", P.LET);
    ("fork", P.FORK);
    ("new", P.NEW);
    ("send", P.SEND);
    ("reveive", P.RECEIVE);
    ("select", P.SELECT);
    ("case", P.CASE);
    ("close", P.CLOSE);
    ("wait", P.WAIT);
  ]

}

rule main = parse
(* ignore whitespace characters *)
| [' ' '\009' '\012' '\n']+     { main lexbuf }

(* integer literal *)
| "-"? ['0'-'9']+
    { P.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| "(" { P.LPAREN }
| ")" { P.RPAREN }
| "{" { P.LBRACE }
| "}" { P.RBRACE }

(* | ";;" { P.SEMISEMI } *)
| "->" { P.RARROW }
| ":" { P.COLON }
| "." { P.PERIOD }

| "+" { P.PLUS }
| "*" { P.STAR }

| "<" { P.LT }
| "=" { P.EQ }

| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try
        List.assoc id reservedWords
      with
      | _ -> Parser.ID id
     }
