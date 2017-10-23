(* Abstract Syntax Tree *)

type op =   Add
          | Sub
          | Mult
          | Div
          | Equal
          | Neq
          | Geq
          | Leq
          | Gt
          | Lt
          | And
          | Or
          | Conc

type uop =   Not
           | Neg
           | Inc
           | Dec

type typ =   Int
           | Float
	   | Object
	   | String
           | Bool
           | Null

type typ_bind = typ * string



type expression = 
	  IntLit of int
        | FloatLit of float
        | StrLit of string
	| BoolLit of bool
	| Id of string
	| Binop of expression * op * expression
	| Unop of uop * expression
	| ObjExp of expression list
	| KeyVal of typ * string * expression
	| ArrExp of expression list
	| Noexpr

type statement = 
	(* *)
	  Block of statement list
	| Expr of expression
	| Assign of string * expression
	| AssignDecl of typ * string * expression
	| ArrAssign of string * expression * expression
	| ArrAssignDecl of typ * string * expression
	| FunExp of string * expression list
	(* *)
        | Return of expression
	| If of expression * statement * statement
	| For of expression * expression * expression * statement
	| While of expression * statement

type function_decl = {
    type_spec : typ;
    f_id : string;
    f_params : typ_bind list;
    f_statements : statement list;
  }

type program = statement list * function_decl list

(* Pretty-printing functions *)

let string_of_op = function
	  Add -> "+"
	| Sub -> "-"
	| Mult -> "*"
	| Div -> "/"
	| Equal -> "=="
	| Neq -> "!="
	| Lt -> "<"
	| Leq -> "<="
	| Gt -> ">"
	| Geq -> ">="
 	| And -> "&&"
	| Or -> "||"
	| Conc -> "^"

let string_of_uop = function
  	  Neg -> "-"
	| Not -> "!"
	| Inc -> "++"
	| Dec -> "--"

let string_of_typ = function
	  Int -> "int"
	| Float -> "float"
	| Object -> "object"
	| String -> "string"
        | Bool -> "bool"
	| Null -> "null"

let rec string_of_expression = function
	  IntLit(i) -> string_of_int i
	| FloatLit(f) -> string_of_float f
	| StrLit(s) -> s
	| BoolLit(true) -> "true"
	| BoolLit(false) -> "false"
	| Id(s) -> s
	| Binop(e1, o, e2) -> string_of_expression e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expression e2
	| Unop(o, e) -> string_of_uop o ^ string_of_expression e
	| ObjExp(el) -> "{\n" ^ String.concat ", " (List.map string_of_expression el) ^ "\n}"
	| KeyVal(t, k, v) -> "  " ^ string_of_typ t ^ " " ^ k ^ " : " ^ string_of_expression v
	| ArrExp(el) -> "[ " ^ String.concat ", " (List.map string_of_expression el) ^ " ]"
  	| Noexpr -> ""

let rec string_of_statement = function
	Block(statements) -> 
		"{\n" ^ String.concat "" (List.map string_of_statement statements) ^ "}\n"
	| Expr(expression) -> string_of_expression expression ^ ";\n";
	| Return(expression) -> "return " ^ string_of_expression expression ^ ";\n";
  	| If(e, s1, s2) ->  "if (" ^ string_of_expression e ^ ")\n" ^ string_of_statement s1 ^ "else\n" ^ string_of_statement s2
  	| For(e1, e2, e3, s) -> "for (" ^ string_of_expression e1  ^ " ; " ^ string_of_expression e2 ^ " ; " ^ string_of_expression e3  ^ ") " ^ string_of_statement s
  	| While(e, s) -> "while (" ^ string_of_expression e ^ ") " ^ string_of_statement s
	| Assign(v, e) -> v ^ " = " ^ string_of_expression e
	| FunExp(f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_expression el) ^ ")"

let string_of_vardec vardec t id =
	string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
	string_of_typ fdecl.type_spec ^ " "
        ^ fdecl.f_id ^ " "
	^ "(" ^ String.concat ", " List.map snd fdecl f_params ^ ")\n{\n"
	^ String.concat "" (List.map string_of_stmts fdecl.f_statements)
	^ "}\n"

let string_of_program stmts fdecls =
	String.concat "" (List.map string_of_statement stmts) ^ "\n"
	^ String.concat "\n" (List.map string_of_fundec funcs)
