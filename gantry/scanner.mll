(* Ocamllex scanner for Gantry *)

{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"        { comment lexbuf }        (* Comments *)
| '('         { LPAREN }
| ')'         { RPAREN }
| '{'         { LBRACE }
| '}'         { RBRACE }
| ';'         { SEMI }
| ','         { COMMA }
| '+'         { PLUS }
| '-'         { MINUS }
| '*'         { TIMES }
| '/'         { DIVIDE }
| '='         { ASSIGN }
| "=="        { EQ }
| "!="        { NEQ }
| '<'         { LT }
| "<="        { LEQ }
| ">"         { GT }
| ">="        { GEQ }
| "&&"        { AND }
| "||"        { OR }
| "!"         { NOT }
| "^"         { CONCAT }                (* String Concatenation *)
| "if"        { IF }
| "elif"      { ELIF }
| "else"      { ELSE }
| "for"       { FOR }
| "while"     { WHILE }
| "continue"  { CONTINUE }
| "break"     { BREAK }
| "return"    { RETURN }
| "int"       { INT }
| "float"     { FLOAT }
| "bool"      { BOOL }
| "null"      { NULL }
| "true"      { TRUE }
| "false"     { FALSE }
| "object"    { OBJECT }
| "arr"       { ARRAY }
| "char"      { CHAR }
| "string"    { STRING }
| "func"      { FUNC }
| "jsonify"   { JSONIFY }
| "objectify" { OBJECTIFY }
| "arrify"    { ARRIFY }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _       { comment lexbuf }
