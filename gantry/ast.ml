(* Abstract Syntax Tree *)


type op = Add
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

type uop = Not
           | Neg
           | Inc
           | Dec

type typ = Int
           | Float
           | Bool
           | Null
           | String

type typ_bind = typ * string

type expression = 
	IntLit of int
        | FloatLit of float
        | StrLit of string
	| BoolLit of bool
	| Id of string
	| Binop of expression * op * expression
	| Unop of uop * expression
	| Assign of string * expression
	| AssignDecl of typ * string * expression
	| ArrAssign of string * expression * expression
	| ArrAssignDecl of typ * string * expression
	| FunExp of string * expression
	| Noexpr

type statement = 
	Block of statement list
	| Expr of expression
	| Return of expression
	| If of expression * statement *statement
	| For of expression * expression * expression
	| While of expression * statement

type function_declaration = {
	type_spec : type_spec;
	f_id : string;
	f_params : func_param_list_opt;
	f_statements = statement_list;
}

type program = statement list * function_declaration list

(* Pretty-printing functions *)

let string_of_op = function
	Add -> "+"
	| Sub -> "-"
	| Mult -> "*"
	| Div -> "/"
	| Equal -> "=="
	| Neq -> "!="
	| Less -> "<"
	| Leq -> "<="
	| Greater -> ">"
	| Geq -> ">="
 	| And -> "&&"
	| Or -> "||"

let string_of_uop = function
	Neg -> "-"
	| Not -> "!"

let rec string_of_expression = function
	Literal(l)
	| BoolLit(true) -> "true"
	| BoolLit(false) -> "false"
	| Id(s) -> s
	| Binop(e1, o, e2) -> string_of_expression e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expression e2
	| Unop(o, e) -> string_of_uop o ^ string_of_expression e
	| Assign(v, e) -> v ^ " = " ^ string_of_expression e
	| FunExp(f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_expression el) ^ ")"
  	| Noexpr -> ""

let rec string_of_statement = function
	Block(statements) -> 
		"{\n" ^ String.concat "" (List.map string_of_statement statements) ^ "}\n"
	| Expr(expression) -> string_of_expression expression ^ ";\n";
	| Return(expression) -> "return " ^ string_of_expression expression ^ ";\n";
  	| If(e, s1, s2) ->  "if (" ^ string_of_expression e ^ ")\n" ^ string_of_statement s1 ^ "else\n" ^ string_of_statement s2
  	| For(e1, e2, e3, s) -> "for (" ^ string_of_expression e1  ^ " ; " ^ string_of_expression e2 ^ " ; " ^ string_of_expression e3  ^ ") " ^ string_of_statement s
  	| While(e, s) -> "while (" ^ string_of_expression e ^ ") " ^ string_of_statement s

let string_of_typ = function
	Int -> "int"
	| Bool -> "bool"
	| Void -> "void"
	| Float -> "float"
	| String -> "string"

let string_of_vardec vardec t id =
	string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fundec fundec
	string_of_typ fundec.type_spec ^ " " ^ fundec.f_id ^ " "
	^ "(" ^ String.concat ", " List.map snd fundec f_params ^ ")\n{\n"
	^ String.concat "" (List.map string_of_vardec fundec.f_statements)
	^ "}\n"

let string_of_program vars funcs =
	String.concat "" (List.map string_of_vardec vars) ^ "\n"
	^ String.concat "\n" (List.map string_of_fundec funcs)
