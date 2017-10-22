/* Ocamlyacc parser for Gantry */

%{
open Ast
%}

/* Tokens / Terminals */
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK SEMI COMMA COLON PERIOD
%token QUOTE
%token PLUS MINUS INCREM DECREM
%token TIMES DIVIDE ASSIGN EQ NEQ
%token LT LEQ GT GEQ
%token AND OR NOT
%token CONCAT
%token IF ELIF ELSE FOR WHILE CONTINUE BREAK RETURN
%token INT FLOAT BOOL NULL OBJECT
%token TRUE FALSE
%token <int> INTLIT
%token <float> FLOATLIT
%token <string> ID STRING STRLIT
%token EOF

/* Precedence Rules */


%right ASSIGN
%left OR
%left AND
%left INCREM DECREM
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS
%right NOT
%left CONCAT

%start program
%type <Ast.program> program

%%

/* CFG */
program:
    declaration_list_opt EOF { $1 }

declaration_list_opt:
    /* empty */                              { [] }
    | declaration_list                       { List.rev $1 }

declaration_list:
    declaration                              { [$1] }
    | declaration_list declaration           { $2 :: $1 }

declaration:
    statement                                { $1 }
    | function_declaration                   { $1 }

/* Use a record as action for semantic checking later */
function_declaration:
    type_spec ID LPAREN func_param_list_opt RPAREN LBRACE statement_list RBRACE
      { { type_spec = $1;
          f_id = $2;
          f_params = $4;
          f_statements = List.rev $7;
      } }

type_spec:
    INT                                     { Int }
    | FLOAT                                 { Float }
    | OBJECT                                { Object }
    | STRING                                { String }
    | BOOL                                  { Bool }
    | NULL                                  { Null }

func_param_list_opt:
     /* nothing */                          { [] }
    | func_param_list                       { List.rev $1 }

func_param_list:
    type_spec ID                            { ($1, $2) }
    | func_param_list COMMA type_spec ID    { ($3, $4) :: $1 }

function_expression:
    | ID LPAREN expression_list_opt RPAREN  { FunExp($1, $3) }

statement_list:
    /* empty */                             { [] }
    | statement_list statement              { $2 :: $1 }

statement:
    for_statement                           { $1 }
    | if_statement                          { $1 }
    | while_statement                       { $1 }
    | jump_statement                        { $1 }
    | expression_statement                  { $1 }

expression_statement:
    expression SEMI                         { $1 }
    | assignment_expression SEMI            { $1 }
    | function_expression SEMI              { $1 }

expression:
    ID                                      { Id($1) }
    | constant                              { $1 }
    | array_expression                      { $1 }
    | object_expression                     { $1 }
    | arithmetic_expression                 { $1 }
    | comparison_expression                 { $1 }
    | logical_expression                    { $1 }
    | string_concat_expression              { $1 }

array_expression:
    LBRACK expression_list_opt RBRACK       { ArrExp($2) }

expression_list_opt:
    /* empty */                             { [] }
    | expression_list                       { List.rev $1 }

expression_list:
    expression                              { $1 }
    | expression_list COMMA expression      { $3 :: $1 }

object_expression:
    LBRACE key_value_list_opt RBRACE        { $2 }

key_value_list_opt:
    /* empty */                             { [] }
    | key_value_list                        { List.rev $1 }

key_value_list:
    key_value                               { $1 }
    | key_value_list COMMA key_value        { $3 :: $1 }

key_value:
    type_spec ID COLON expression 	    { KeyVal($2, $4) }

arithmetic_expression:
    expression PLUS expression              { Binop($1, Add, $3) }
    | expression MINUS expression           { Binop($1, Sub, $3) }
    | expression TIMES expression           { Binop($1, Mult, $3) }
    | expression DIVIDE expression          { Binop($1, Div, $3) }
    | expression INCREM                     { Unop(Inc, $1) }
    | expression DECREM                     { Unop(Dec, $1) }
    | MINUS expression %prec UMINUS 	    { Unop(Neg, $2) }

comparison_expression:
    expression LT expression                { Binop($1, Lt, $3) }
    | expression GT expression              { Binop($1, Gt, $3) }
    | expression LEQ expression             { Binop($1, Leq, $3) }
    | expression GEQ expression             { Binop($1, Geq, $3) }
    | expression EQ expression              { Binop($1, Eq, $3) }
    | expression NEQ expression             { Binop($1, Neq, $3) }

logical_expression:
    expression AND expression               { Binop($1, And, $3) }
    | expression OR expression              { Binop($1, Or, $3) }
    | NOT expression                        { Unop($2) }

string_concat_expression:
    expression CONCAT expression            { StrConc($1, Ct, $3) }

assignment_expression:
    ID ASSIGN expression                        { Assign($1, $3) }
    | type_spec ID ASSIGN expression            { AssignDecl($1, $2, $4) }
    | ID LBRACK expression RBRACK ASSIGN expression
        { ArrAssign($1, $3, $6) }
    | type_spec LBRACK RBRACK ID ASSIGN expression
        { ArrAssignDecl($1, $4, $6) }

for_statement:
    FOR LPAREN expression SEMI expression SEMI expression SEMI RPAREN LBRACE statement_list RBRACE
      { For($3, $5, $7) }

if_statement:
    IF LPAREN expression RPAREN LBRACE statement_list RBRACE
      { If($3, $6) }
    | IF LPAREN expression RPAREN LBRACE statement_list RBRACE ELSE LBRACE statement_list RBRACE
      { If($3, $6, $10) }
    | IF LPAREN expression RPAREN LBRACE statement_list RBRACE
      ELIF LPAREN expression  RPAREN LBRACE statement_list RBRACE ELSE LBRACE statement_list RBRACE
      { If($3, $6, $10, $13, $17) }

while_statement:
    WHILE LPAREN expression RPAREN LBRACE statement_list RBRACE
      { While($3, $6) }

jump_statement:
    BREAK SEMI                              { Break }
    | CONTINUE SEMI                         { Continue }
    | RETURN expression SEMI                { Return($2) }

constant:
    TRUE                                    { BoolLit(true) }
    | FALSE                                 { BoolLit(false) }
    | NULL                                  { Null }
    | literal                               { $1 }

literal:
    INTLIT                                  { IntLit($1) }
    | FLOATLIT                              { FloatLit($1) }
    | STRLIT                                { StrLit($1) }

