(* Gantry Semantic Abstract Syntax Tree - SAST *)
(* reference Gantry AST, and sake SAST *)
(* Semantic checking *)

type op =
  | Add | Sub | Mult | Div
  | Eq | Neq | Geq | Leq | Gt | Lt
  | And | Or | Conc

type uop =
  Not | Neg | Inc | Dec

type typ =
  Int | Float | Object | Array | String | Bool | Null

type typ_bind = typ * string

type expression =
          IntLit of int
        | FloatLit of float
        | StrLit of string
        | BoolLit of bool
        | Id of string
        | ObjAcc of expression * expression
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

type program = typ_bind list * function_decl list

(* end of sast *)
