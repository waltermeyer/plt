(* Abstract Syntax Tree *)


type op = Add | Sub | Mult | Div | Equal | Neq |Geq | Leq | Gt| Lt | And | Or

type uop = Not

type typ = Int | Float | Bool | Null

type expr = 
	Intlit of int
	| BoolLit of bool
	| Id of string
	| Binop of expr * op * expr
	| Unop of uop * expr
	| Assign of string * expr
	| Call of string * expr
	| Noexpr

type stmt = 
	Block of stmt list
	| Expr of expr
	| Return of expr
	| If of expr * stmt *stmt
	| For of expr * expr * expr
	| While of expr * stmt

type function_declaration = {
	type_spec : type_spec;
	f_id : string;
	f_params : func_param_list_opt;
	f_statements = statement_list;
}

type program = bind list * function_declaration list
