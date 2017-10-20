(* Abstract Syntax Tree *)


type op = Add | Sub | Mult | Div | Equal | Neq |Geq | Leq | Gt| Lt | And | Or

type uop = Not

type typ = Int | Float | Bool | Null

type expr = 
	Literal of int
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
