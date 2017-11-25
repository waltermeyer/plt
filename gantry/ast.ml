(* Abstract Syntax Tree *)

type op =
  | Add | Sub | Mult | Div
  | Eq | Neq | Geq | Leq | Gt | Lt
  | And | Or | Conc

type uop =
  Not | Neg | Inc | Dec

type typ =
    Int | Float | Object | String | Bool | Null |
    Int_Array | Float_Array | Object_Array | String_Array | Bool_Array

type typ_bind = typ * string

type expression = 
	  IntLit of int
        | FloatLit of float
        | StrLit of string
	| BoolLit of bool
	| Id of string
	| ArrAcc of expression * expression
	| Binop of expression * op * expression
	| Unop of uop * expression
	| Assign of expression * expression
	| AssignDecl of typ * string * expression
	| ObjAssign of expression list * expression
	| FunExp of string * expression list
	| KeyVal of typ * string * expression
	| ArrExp of expression list
	| ObjExp of expression list
	| Noexpr

type statement = 
	  Block of statement list
	| Expr of expression
        | Return of expression
	| If of expression * statement * statement
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

type program = typ_bind list * function_decl list


(* Expression to String for Variable Resolution in Codegen *)
let expr_to_str = function
	  Id(s) -> s
	| _     -> ""

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
	| Int_Array -> "int array"
	| Float_Array -> "float array"
	| Object_Array -> "object array"
	| String_Array -> "string array"
        | Bool_Array -> "bool array"

let rec string_of_expression = function
	  IntLit(i) -> string_of_int i
	| FloatLit(f) -> string_of_float f
	| StrLit(s) -> "\"" ^ s ^ "\""
	| BoolLit(true) -> "true"
	| BoolLit(false) -> "false"
	| Id(s) -> s
	| ArrAcc(e1, e2) -> string_of_expression e1 ^ "[" ^ string_of_expression e2 ^ "]"
	| Binop(e1, o, e2) -> string_of_expression e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expression e2
	| Unop(o, e) -> (match o with
			Inc | Dec -> string_of_expression e ^ string_of_uop o
			| _ -> string_of_uop o ^ string_of_expression e)
	| KeyVal(t, k, e) -> "  " ^ string_of_typ t ^ " " ^ k ^ " : " ^ string_of_expression e
	| ArrExp(el) -> "[ " ^ String.concat ", " (List.map string_of_expression el) ^ " ]"
	| ObjExp(el) -> "{| " ^ String.concat ", " (List.map string_of_expression el) ^ " |}"
	| Assign(e1, e2) -> string_of_expression e1 ^ " = " ^ string_of_expression e2
	| AssignDecl(t, v, e) -> string_of_typ t ^ " " ^ v ^ " = " ^ string_of_expression e
	| ObjAssign(el, e) -> String.concat "." (List.map string_of_expression el) ^ " = " ^ string_of_expression e
	| FunExp(f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_expression el) ^ ")"
  	| Noexpr -> ""

let rec string_of_statement = function
	  Block(statements) ->
		"{\n" ^ String.concat "" (List.map string_of_statement statements) ^ "}\n"
	| Expr(expression) -> string_of_expression expression ^ ";\n";
	| Return(expression) -> "return " ^ string_of_expression expression ^ ";\n";
	| If(e1, s1, Block([])) -> "if (" ^ string_of_expression e1 ^ ")\n" ^
                                                      string_of_statement s1 ^ "\n"
	| If(e1, s1, s2) -> "if (" ^ string_of_expression e1 ^ ")\n" ^
					       string_of_statement s1 ^ "\n" ^
                                               "else\n" ^ string_of_statement s2 ^ "\n"
	| For(e1, e2, e3, s) -> "for (" ^ string_of_expression e1  ^ " ; " ^ string_of_expression e2 ^ " ; "
				        ^ string_of_expression e3  ^ ") " ^ string_of_statement s
	| While(e, s) -> "while (" ^ string_of_expression e ^ ")\n" ^ string_of_statement s ^ "\n"
	| Break -> "break ; "
	| Continue -> "continue ; "

let string_of_global (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_typ_bind (t, id) = string_of_typ t ^ " " ^ id

let string_of_fdecl fdecl =
	string_of_typ fdecl.type_spec ^ " "
        ^ fdecl.f_id ^ " " ^ "(" ^ String.concat ", " (List.map string_of_typ_bind fdecl.f_params) ^ ")\n{\n"
	^ String.concat "" (List.map string_of_statement fdecl.f_statements)
	^ "}\n"

let string_of_program (globals, fdecls) =
	String.concat "" (List.map string_of_global globals) ^ "\n"
	^ String.concat "\n" (List.map string_of_fdecl fdecls)
