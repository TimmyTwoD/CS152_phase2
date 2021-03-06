    /* cs152-miniL phase2 */
%{
 #include <stdio.h>
 #include <stdlib.h>

 void yyerror(const char *msg);
 extern int currLine; 
 extern int currPos; 
 FILE * yyin;
%}

%union{
  /* put your types here */
  double dval;
  char* str;
}

%error-verbose
%start prog_start
%token FUNCTION SEMICOLON BEGIN_PARAMS END_PARAMS BEGIN_LOCALS END_LOCALS BEGIN_BODY END_BODY INTEGER ARRAY ENUM OF IF THEN ENDIF ELSE FOR WHILE DO BEGINLOOP ENDLOOP CONTINUE READ WRITE AND OR NOT TRUE FALSE RETURN COLON COMMA NUMBER

  /* %types functions function declarations declaration identifiers ident statements statement bool_exp relation_and_exp relation_exp comp expression expressions term vars var multiplicative_expression */
%right ASSIGN ":="
%left OR "|"
%left AND "&"
%right NOT "!"
%left LT "<" LTE "<=" GT ">" GTE ">=" EQ "=" NEQ "!="
%left ADD "+" SUB "-"
%left MULT "*" DIV "/" MOD "%"
    /* %right minus "-" */
%left L_SQUARE_BRACKET "[" R_SQUARE_BRACKET "]"
%left L_PAREN "(" R_PAREN ")"
%token <dval> DIGIT
%token <str> IDENT

/* %start program */

%% 

    /* write your rules here */
    prog_start:
        functions {printf("prog_start -> functions\n");}
    ;

    functions:
        {printf("functions -> epsilon\n");}
        |function functions {printf("functions -> function functions\n");}
    ;

    function:
        FUNCTION ident SEMICOLON BEGIN_PARAMS declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY {printf("function -> FUNCTION IDENT SEMICOLON BEGIN_PARAMS declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY\n");}
    ;

    declarations:
        declaration SEMICOLON declarations {printf("declarations -> declaration SEMICOLON declarations\n");}
        |{printf("declarations -> epsilon\n");}
    ;

    declaration:
        identifiers COLON INTEGER {printf("declaration -> identifiers COLON INTEGER\n");}
        |identifiers COLON ENUM L_PAREN identifiers R_PAREN {printf("declaration -> identifiers COLON ENUM L_PAREN identifiers R_PAREN\n");}
        |identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER {printf("declaration -> identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER\n");}
    ;

    identifiers:
        ident {printf("identifiers -> ident\n");}
        |ident COMMA identifiers {printf("identifiers -> ident COMMA identifiers\n");}
    ;

    ident:
        IDENT {printf("ident -> IDENT %s\n", $1);}
    ;

    /*The looping things*/
    statements:
        statement SEMICOLON statements {printf("statements -> statement SEMICOLON statements\n");}
        |{printf("statements -> epsilon\n");}
    ;

    statement:
        var ASSIGN expression {printf("statement -> var ASSIGN expression\n");}
        |IF bool_exp THEN statements ENDIF {printf("statement -> IF bool_exp THEN statements ENDIF\n");}
        |IF bool_exp THEN statements ELSE statements ENDIF {printf("statement -> IF bool_exp THEN statements ELSE statements ENDIF\n");}
        |WHILE bool_exp BEGINLOOP statements ENDLOOP {printf("statement -> WHILE bool_exp BEGINLOOP statements ENDLOOP\n");}
        |DO BEGINLOOP statements ENDLOOP WHILE bool_exp {printf("statement -> DO BEGINLOOP statements ENDLOOP WHILE bool_exp\n");}
        |READ vars {printf("statement -> READ vars\n");}
        |WRITE vars {printf("statement -> WRITE vars\n");}
        |CONTINUE {printf("statement -> CONTINUE\n");}
        |RETURN expression {printf("statement -> RETURN expression\n");}
    ;

    bool_exp:
        relation_and_exp {printf("bool_exp -> relation_and_exp\n");}
        |relation_and_exp OR relation_and_exp {printf("bool_exp -> relation_and_exp OR relation_and_exp\n");}
    ;

    relation_and_exp:
        relation_exp {printf("relation_and_exp -> relation_exp\n");}
        |relation_and_exp AND relation_exp {printf("relation_and_exp -> relation_exp AND relation_exp\n");}
    ;

    relation_exp:
        expression comp expression {printf("relation_exp -> expression comp expression\n");}
        |TRUE {printf("relation_exp -> TRUE\n");}
        |FALSE {printf("relation_exp -> FALSE\n");}
        |L_PAREN bool_exp R_PAREN {printf("relation_exp -> L_PAREN bool_exp R_PAREN\n");}
        |NOT expression comp expression {printf("relation_exp -> NOT expression comp expression\n");}
        |NOT TRUE {printf("relation_exp -> NOT TRUE\n");}
        |NOT FALSE {printf("relation_exp -> NOT FALSE\n");}
        |NOT L_PAREN bool_exp R_PAREN {printf("relation_exp -> NOT L_PAREN bool_exp R_PAREN\n");}
    ;

    comp:
        EQ {printf("comp -> EQ\n");}
        |NEQ {printf("comp -> NEQ\n");}
        |LT {printf("comp -> LT\n");}
        |GT {printf("comp -> GT\n");}
        |LTE {printf("comp -> LTE\n");}
        |GTE {printf("comp -> GTE\n");}
    ;

    expression:
        multiplicative_expression {printf("expression -> multiplicative_expression\n");}
        |expression SUB multiplicative_expression {printf("expression -> multiplicative_expression SUB multiplicative_expression\n");}
        |expression ADD multiplicative_expression {printf("expression -> multiplicative_expression ADD multiplicative_expression\n");}
    ;

    expressions:
        {printf("expressions -> epsilon\n");}
        |expression COMMA expressions {printf("expressions -> expression COMMA expressions\n");} 
        |expression {printf("expressions -> expression\n");}
    ;

    multiplicative_expression:
        term {printf("multiplicative_expression -> term\n");}
        |term DIV term {printf("multiplicative_expression -> term DIV term\n");}
        |term MULT term {printf("multiplicative_expression -> term MULT term\n");} 
        |term MOD term {printf("multiplicative_expression -> term MOD term\n");}
    ;

    term:
        var {printf("term -> var\n");}
        |NUMBER {printf("term -> NUMBER\n");}
        |L_PAREN expression R_PAREN {printf("term -> L_PAREN expression R_PAREN\n");}
        |SUB var {printf("term -> SUB var\n");}
        |SUB NUMBER {printf("term -> SUB NUMBER\n");}
        |SUB L_PAREN expression R_PAREN {printf("term -> SUB L_PAREN expression R_PAREN\n");}
        |ident L_PAREN expressions R_PAREN {printf("term -> ident L_PAREN expressions R_PAREN\n");}
    ;

    vars:
        var COMMA vars {printf("vars -> var COMMA vars\n");}
        |var {printf("vars -> var\n");}
    ;

    var:
        ident {printf("var -> ident\n");}
        |ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET {printf("var -> ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET\n");}
    ;

    
  

%% 

int main(int argc, char **argv) {
    if(argc >= 2){
      yyin = fopen(argv[1], "r");
      if(yyin == NULL){
         yyin = stdin;
      }
    }
    else{
      yyin = stdin;
    }
    yyparse();
    return 0;
}

void yyerror(const char *msg) {
    printf("Error: On line %d, column %d: %s \n", currLine, currPos, msg);
    
}