{
module P = Parser

let reservedWords = [
    ("lin", P.LIN);
    ("un", P.UN);
    ("unit", P.UNIT);           (* types *)
    ("int", P.INT);
    ("bool", P.BOOL);
    ("end", P.END);
    ("true", P.TRUE);           (* bool *)
    ("false", P.FALSE);
    ("if", P.IF);
    ("then", P.THEN);
    ("else", P.ELSE);
    ("fun", P.FUN);
    ("let", P.LET);
    ("in", P.IN);
    ("fork", P.FORK);           (* communication *)
    ("new", P.NEW);
    ("send", P.SEND);
    ("receive", P.RECEIVE);
    ("select", P.SELECT);
    ("case", P.CASE);
    ("of", P.OF);
    ("close", P.CLOSE);
    ("wait", P.WAIT);
  ]

}

rule main = parse
(* ignore whitespace characters *)
| [' ' '\009' '\012' '\n']+     { main lexbuf }

(* integer literal *)
| ['0'-'9']+
    { P.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| "(*" { comment 0 lexbuf }     (* Entering comment mode *)
| ";;" { P.SEMISEMI }

| "(" { P.LPAREN }
| ")" { P.RPAREN }
| "{" { P.LBRACE }
| "}" { P.RBRACE }

| "->" { P.RARROW }
| "!" { P.BANG }
| "?" { P.QU }                  (* question mark *)
| "&" { P.AMP }                 (* ampersand *)
| ":" { P.COLON }
| ";" { P.SEMI }
| "." { P.PERIOD }
| "," { P.COMMA }

| "+" { P.PLUS }
| "-" { P.MINUS }
| "*" { P.STAR }
| "/" { P.SLASH }

| "=" { P.EQ }
| "<" { P.LT }
| ">" { P.GT }
| "<=" { P.LE }
| ">=" { P.GE }

| "Dyn" { P.DYN }
| "DC" { P.DC }

| ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try
        List.assoc id reservedWords
      with
      | _ -> Parser.ID id
     }

and comment level = parse
| "*)" {
  if level = 0 then main lexbuf
  else comment (level - 1) lexbuf
  }
| "(*" { comment (level + 1) lexbuf }
| _ { comment level lexbuf }    (* ignore any char *)
| eof { failwith "Comment is not closed." }
