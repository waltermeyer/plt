/* Ocamlyacc parser for Gantry */

%{
open Ast
%}

/* Tokens / Terminals */
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK SEMI COLON COMMA PERIOD BAR
%token QUOTE
%token PLUS MINUS INCREM DECREM
%token TIMES DIVIDE ASSIGN EQ NEQ
%token LT LEQ GT GEQ
%token AND OR NOT
%token CONCAT
%token IF ELIF ELSE FOR WHILE CONTINUE BREAK RETURN
%token INT FLOAT OBJECT ARRAY STRING BOOL NULL
%token TRUE FALSE
%token <int> INTLIT
%token <float> FLOATLIT
%token <string> ID STRLIT
%token EOF

/* Precedence Rules */
%nonassoc NOELSE
%nonassoc ELIF
%nonassoc ELSE
%right ASSIGN COLON
%left PERIOD LBRACK
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
    declaration_list EOF 		     { (List.rev (fst $1)), (List.rev (snd $1)) }

/* Build up a tuple of ordered lists for stmts and fdecls for the AST */
declaration_list:
    /* empty */                              { [], [] }
    | declaration_list global_declaration    { $2 :: fst $1, snd $1 }
    | declaration_list function_declaration  { fst $1, $2 :: snd $1 }

global_declaration:
    type_spec ID SEMI                        { ($1, $2) }

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
    | ARRAY                                 { Array }
    | STRING                                { String }
    | BOOL                                  { Bool }
    | NULL                                  { Null }

func_param_list_opt:
     /* nothing */                          { [] }
    | func_param_list                       { List.rev $1 }

func_param_list:
    type_spec ID                            { [($1, $2)] }
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
    | LBRACE statement_list RBRACE	    { Block(List.rev $2) }

expression_statement:
    expression SEMI                         { Expr($1) }

expression:
    ID		                            { Id($1) }
    | access_expression			    { $1 }
    | constant                              { $1 }
    | array_expression                      { $1 }
    | object_expression			    { $1 }
    | arithmetic_expression                 { $1 }
    | comparison_expression                 { $1 }
    | logical_expression                    { $1 }
    | string_concat_expression              { $1 }
    | assignment_expression                 { $1 }
    | function_expression                   { $1 }

array_expression:
    LBRACK expression_list_opt RBRACK       { ArrExp($2) }

access_expression:
    expression LBRACK expression RBRACK     { ArrAcc($1, $3) }

assignment_expression:
    expression ASSIGN expression   	    { Assign($1, $3) }
    | type_spec ID ASSIGN expression        { AssignDecl($1, $2, $4) }
    | type_spec ID COLON expression         { KeyVal($1, $2, $4) }

object_expression:
    LBRACE BAR expression_list_opt BAR RBRACE
	{ ObjExp($3) }

expression_list_opt:
    /* empty */                             { [] }
    | expression_list                       { List.rev $1 }

expression_list:
    expression                              { [$1] }
    | expression_list COMMA expression      { $3 :: $1 }

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
    | NOT expression                        { Unop(Not, $2) }

string_concat_expression:
    expression CONCAT expression            { Binop($1, Conc, $3) }

for_statement:
    FOR LPAREN expression SEMI expression SEMI expression RPAREN statement
      { For($3, $5, $7, $9) }

if_statement:
    IF LPAREN expression RPAREN statement %prec NOELSE
      { If($3, $5, Noexpr, Block([]), Block([])) }
    | IF LPAREN expression RPAREN statement ELSE statement
      { If($3, $5, Noexpr, Block([]), $7) }
    | IF LPAREN expression RPAREN statement
      ELIF LPAREN expression RPAREN statement %prec NOELSE
      { If($3, $5, $8, $10, Block([])) }
    | IF LPAREN expression RPAREN statement
      ELIF LPAREN expression RPAREN statement ELSE statement
      { If($3, $5, $8, $10, $12) }

while_statement:
    WHILE LPAREN expression RPAREN statement
      { While($3, $5) }

jump_statement:
    BREAK SEMI                              { Break }
    | CONTINUE SEMI                         { Continue }
    | RETURN SEMI	                    { Return(Noexpr) }
    | RETURN expression SEMI                { Return($2) }

constant:
    TRUE                                    { BoolLit(true) }
    | FALSE                                 { BoolLit(false) }
    | literal                               { $1 }

literal:
    INTLIT                                  { IntLit($1) }
    | FLOATLIT                              { FloatLit($1) }
    | STRLIT                                { StrLit($1) }
