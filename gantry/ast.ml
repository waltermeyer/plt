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
	| BootLit of bool
	| Id of string
	| Binop of expression * op * expression
	| Unop of uop * expression
	| Assign of string * expression
	| Call of string * expression
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

type program = typ_bind list * function_declaration list
