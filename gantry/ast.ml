(* Abstract Syntax Tree *)

type op =   Add
          | Sub
          | Mult
          | Div
          | Eq
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
	| NullLit of string
	| Id of string
	| ArrId of string * expression
	| Binop of expression * op * expression
	| Unop of uop * expression
	| Assign of string * expression
	| AssignDecl of typ * string * expression
	| ArrAssign of string * expression * expression
	| ArrAssignDecl of typ * string * expression
	| FunExp of string * expression list
	| ObjExp of expression list
	| KeyVal of typ * string * expression
	| ArrExp of expression list
	| Noexpr

type statement = 
	  Block of statement list
	| Expr of expression
        | Return of expression
	| If of expression * statement * expression * statement * statement
	| For of expression * expression * expression * statement
	| While of expression * statement
	| Break
	| Continue

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
	| Eq -> "=="
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
	| StrLit(s) -> "\"" ^ s ^ "\""
	| BoolLit(true) -> "true"
	| BoolLit(false) -> "false"
	| NullLit(s) -> s
	| Id(s) -> s
	| ArrId(s, e) -> s ^ "[" ^ string_of_expression e ^ "]"
	| Binop(e1, o, e2) -> string_of_expression e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expression e2
	| Unop(o, e) -> string_of_uop o ^ string_of_expression e
	| ObjExp(el) -> "{\n" ^ String.concat ", " (List.map string_of_expression el) ^ "\n}"
	| KeyVal(t, k, e) -> "  " ^ string_of_typ t ^ " " ^ k ^ " : " ^ string_of_expression e
	| ArrExp(el) -> "[ " ^ String.concat ", " (List.map string_of_expression el) ^ " ]"
	| Assign(v, e) -> v ^ " = " ^ string_of_expression e
	| AssignDecl(t, v, e) -> string_of_typ t ^ " " ^ v ^ " = " ^ string_of_expression e
	| ArrAssign(v, e1, e2) -> v ^ "[ " ^ string_of_expression e1 ^ " ]" ^ " = " ^ string_of_expression e2
	| ArrAssignDecl(t, v, e) -> string_of_typ t ^ " [] " ^ v ^ " = " ^ string_of_expression e
	| FunExp(f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_expression el) ^ ")"
  	| Noexpr -> ""

let rec string_of_statement = function
	  Block(statements) ->
		"{\n" ^ String.concat "" (List.map string_of_statement statements) ^ "}\n"
	| Expr(expression) -> string_of_expression expression ^ ";\n";
	| Return(expression) -> "return " ^ string_of_expression expression ^ ";\n";
	| If(e1, s1, Noexpr, Block([]), Block([])) -> "if (" ^ string_of_expression e1 ^ ")\n" ^
                                                      "{\n" ^ "  " ^ string_of_statement s1 ^ "}\n"
	| If(e1, s1, Noexpr, Block([]), s2) -> "if (" ^ string_of_expression e1 ^ ")\n" ^
					       "{\n" ^ "  " ^ string_of_statement s1 ^ "}\n" ^
                                               "else\n" ^ "{\n" ^ "  " ^ string_of_statement s2 ^ "}\n"
	| If(e1, s1, e2, s2, Block([])) -> "if (" ^ string_of_expression e1 ^ ")\n" ^
				           "{\n" ^ "  " ^ string_of_statement s1 ^ "}\n" ^
					   "elif (" ^ string_of_expression e2 ^ ")\n" ^
					   "{\n" ^ "  " ^ string_of_statement s2 ^ "}\n"
	| If(e1, s1, e2, s2, s3) -> "if (" ^ string_of_expression e1 ^ ")\n" ^
				    "{\n" ^ "  " ^ string_of_statement s1 ^ "}\n" ^
				    "elif (" ^ string_of_expression e2 ^ ")\n" ^
				    "{\n" ^ "  " ^ string_of_statement s2 ^ "}\n" ^
			            "else\n" ^ "{\n" ^ "  " ^ string_of_statement s3 ^ "}\n"
	| For(e1, e2, e3, s) -> "for (" ^ string_of_expression e1  ^ " ; " ^ string_of_expression e2 ^ " ; "
				        ^ string_of_expression e3  ^ ") " ^ string_of_statement s
	| While(e, s) -> "while (" ^ string_of_expression e ^ ")\n" ^ "{\n" ^ string_of_statement s ^ "}\n"
	| Break -> "break ; "
	| Continue -> "continue ; "

let string_of_fdecl fdecl =
	string_of_typ fdecl.type_spec ^ " "
        ^ fdecl.f_id ^ " " ^ "(" ^ String.concat ", " (List.map snd fdecl.f_params) ^ ")\n{\n"
	^ String.concat "" (List.map string_of_statement fdecl.f_statements)
	^ "}\n"

let string_of_program (stmts, fdecls) =
	String.concat "" (List.map string_of_statement stmts) ^ "\n"
	^ String.concat "\n" (List.map string_of_fdecl fdecls)
