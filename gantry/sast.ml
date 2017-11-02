(* Gantry SAST *)
(* reference sake and QL *)

type op = Add | Sub | Mul | Div | Eq | Neq | Lt | Le | Gt | Ge | And | Or
type uop = Neg | Not
type dtype = (* built-in primitives + custom user type *)
	| Bool | Int | Char | String
	| Enum of string (* name of the enum *)
type expr = 
	| BoolLit of bool
	| CharLit of char
	| IntLit of int
	| StringLit of string
	| Variable of string
	| Uop of uop * expr
	| Binop of expr * op * expr
	| Assign of string * expr
	| Print of string * expr list
	| Empty
type stmt = 
	| Block of stmt list
	| State of string
	| If of expr * stmt * stmt
	| For of string * (int * int * int) * stmt
	| While of expr * stmt
	| Switch of expr * (expr * stmt list) list
	| Expr of expr
	| Goto of string (* for FSM transitions *)
	| Halt
type type_decl = {
	type_name : string;
	type_values : string list;
	}
type fsm_decl = {
	fsm_name : string;
	fsm_states: (string * int) list;
	fsm_locals: (dtype * string * expr) list;
	fsm_body: stmt list;
	}
type program = {
	input : (dtype * string) list;
	output: (dtype * string) list;
	public: (dtype * string * expr) list;
	types: type_decl list;
	fsms : fsm_decl list;
	}
type variable_decl = (string * dtype)
type symbol_table = {
	parent : symbol_table option;
	mutable variables : variable_decl list
	}
type translation_environment = {
	score : symbol_table; (* symbold table for vars *)
}
