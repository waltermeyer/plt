(*
 * Gantry: Scanner
 * Author: Walter Meyer
 *)

{ open Parser }

let identifier = ['a'-'z' 'A'-'Z' '0'-'9']['a'-'z' 'A'-'Z' '0'-'9' '_' '.']*

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"        { comment lexbuf }        (* Comments *)
| "//"        { new_comment lexbuf }    (* New-Style Comments *)
| '('         { LPAREN }
| ')'         { RPAREN }
| '{'         { LBRACE }
| '}'         { RBRACE }
| '['         { LBRACK }
| ']'         { RBRACK }
| ';'         { SEMI }
| ','         { COMMA }
| ':'         { COLON }
| '.'         { PERIOD }
| '|'	      { BAR }

(* Arithmetic Operators (Binary and Unary) *)
| '+'         { PLUS }
| '-'         { MINUS }
| "++"        { INCREM }
| "--"        { DECREM }

(* Arithmetic Operators (Binary) *)
| '*'         { TIMES }
| '/'         { DIVIDE }
| '='         { ASSIGN }
| "=="        { EQ }
| "!="        { NEQ }

(* Relational Operators *)
| '<'         { LT }
| "<="        { LEQ }
| ">"         { GT }
| ">="        { GEQ }

(* Logical Operators *)
| "&&"        { AND }
| "||"        { OR }
| "!"         { NOT }

(* String Concatenation *)
| "^"         { CONCAT }

(* Flow Control *)
| "if"        { IF }
| "else"      { ELSE }
| "for"       { FOR }
| "while"     { WHILE }
| "continue"  { CONTINUE }
| "break"     { BREAK }
| "return"    { RETURN }

(* Keywords *)
| "int"       { INT }
| "float"     { FLOAT }
| "bool"      { BOOL }
| "null"      { NULL }
| "true"      { TRUE }
| "false"     { FALSE }
| "object"    { OBJECT }
| "array"     { ARRAY }
| "string"    { STRING }

(* Strings *)
| '"'         { read_string (Buffer.create 10) lexbuf }

(* Literals *)
| ['0'-'9']+ as lxm { INTLIT(int_of_string lxm) }
| ['0'-'9']+['.']['0'-'9']+ as lxm { FLOATLIT(float_of_string lxm) }

(* Identifiers *)
| identifier as lxm { ID(lxm) }

(* EOF and Error Handling *)
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

(* Comments *)
and comment = parse
  "*/" { token lexbuf }
| _       { comment lexbuf }

and new_comment = parse
  '\n' { token lexbuf }
| _       { new_comment lexbuf }

(* String Literals
 * Recursive read_string modified from
 * https://realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html
 * accept '\r', '\t', '\n', '\b', '\f', '\"', '\\'
 *)
and read_string buf =
  parse
  | '"'       { STRLIT (Buffer.contents buf) }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' '"'  { Buffer.add_char buf '\"'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (Failure ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (Failure ("String is not terminated")) }

