(* Literals. *)
%token <string> ID
%token <int> INT
%token <string> STRING

(* Single-char tokens. *)
%token LPAREN RPAREN LBRACE RBRACE
%token PLUS MINUS SLASH STAR COMMA DOT SEMICOLON

(* One or two character tokens. *)
%token BANG, BANG_EQUAL
%token EQUAL, EQUAL_EQUAL
%token GREATER, GREATER_EQUAL
%token LESS, LESS_EQUAL

(* Keywords. *)
%token VAR CLASS SUPER
%token AND OR
%token TRUE FALSE
%token IF ELSE 
%token FOR WHILE
%token FUN RETURN NIL
%token PRINTF
%token EOF

%start <unit> prog
%%

prog:
    | EOF { () }