/* Ocamlyacc parser for Gantry */

%{
open Ast
%}

/* Tokens / Terminals */
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK SEMI COMMA COLON PERIOD
%token PLUS MINUS INCREM DECREM
%token TIMES DIVIDE ASSIGN EQ NEQ
%token LT LEQ GT GEQ
%token AND OR NOT
%token CONCAT
%token IF ELIF FOR WHILE CONTINUE BREAK RETURN
%token INT FLOAT BOOL NULL OBJECT
%token <string> ID STRING
%token TRUE FALSE
%token <int> INTLIT
%token <float> FLOATLIT
%token EOF

/* Precedence Rules */
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG
%left CONCAT

start program
%type <Ast.program> program

%%

/* CFG */
program:
      declaration-list EOF { (List.rev $1) }

declaration_list:
     /* empty */                             { [] }
    | declaration_list declaration           { $2 :: $1 }

declaration:
    | statement                              { $1 }
    | function_declaration                   { $1 }

/* Use a record as action for semantic checking later */
function_declaration:
    | type_spec ID LPAREN func_param_list_opt RPAREN RBRACE statement_list LBRACE
      { { type_spec = $1;
          f_id = $2;
          f_params = $4;
          f_body = List.rev $7;
      } }

/* Explicit typed-arrays missing and should be semantically checked. */
type_spec
    | INT                                   { Int }
    | FLOAT                                 { Float }
    | BOOL                                  { Bool }
    | NULL                                  { Null }
    | OBJECT                                { Object }
    | STRING                                { String }

func_param_list_opt:
    /* nothing */ { [] }
    | func_param_list   { List.rev $1 }

func_param_list:
    | type_spec ID                          { [] }
    | func_param_list COMMA type_spec ID    { [] }

statement_list:

statement_list:
     /* empty */                            { [] }
    | statement_list statement              { $2 :: $1 }

statement:
      for_statement         { $1 }
    | if_statement          { $1 }
    | while_statement       { $1 }
    | jump_statement        { $1 }
    | expression_statement  { $1 }






