%{
open Ast
open Lexing

let make_located value start_pos = 
  { value; pos = { line = start_pos.pos_lnum; 
                   column = start_pos.pos_cnum - start_pos.pos_bol } }

type top_level_item = 
  | Function of func_def
  | Statement of stmt
%}

(* Literals. *)
%token <string> ID
%token <float> FLOAT
%token <string> STRING

(* Single-char tokens. *)
%token LPAREN RPAREN LBRACE RBRACE
%token PLUS MINUS SLASH STAR COMMA DOT SEMICOLON

(* One or two character tokens. *)
%token BANG BANG_EQUAL
%token EQUAL EQUAL_EQUAL
%token GREATER GREATER_EQUAL
%token LESS LESS_EQUAL

(* Keywords. *)
%token VAR
%token AND OR
%token TRUE FALSE
%token IF ELSE 
%token FOR WHILE
%token FUN RETURN NULL
%token PRINTF
%token EOF

%left OR
%left AND
%left EQUAL_EQUAL BANG_EQUAL
%left LESS LESS_EQUAL GREATER GREATER_EQUAL
%left PLUS MINUS
%left STAR SLASH
%right BANG

%start <Ast.program> program

%%

program: 
  | items = list(top_level_item) EOF {
    let functions = List.filter_map (function
      | Function f -> Some f
      | _ -> None) items in
    let main_stmts = List.filter_map (function
      | Statement s -> Some s
      | _ -> None) items in
    { functions; main = main_stmts }
  }

top_level_item:
  | f = function_def { Function f }
  | s = statement { Statement s }

function_def: 
  | FUN name = ID LPAREN params = param_list
               RPAREN LBRACE body = list(statement) RBRACE {
      let name_loc = make_located name $startpos(name) in
      { name = name_loc; params; body }
  }

param_list:
  | { [] }
  | p = ID { [make_located p $startpos(p)] }
  | p = ID COMMA rest = param_list {
      make_located p $startpos(p) :: rest
    }

statement: 
  | e = expression SEMICOLON { Expression e }
  | id = ID EQUAL e = expression SEMICOLON {
      Assignment (make_located id $startpos(id), e)
    } 
  | VAR id = ID EQUAL e = expression? SEMICOLON {
      VarDeclaration (make_located id $startpos(id), e)
    }
  | IF LPAREN cond = expression RPAREN then_stmt = block_or_single 
    else_part = else_clause? {
      If (cond, then_stmt, else_part)    
    }
  | WHILE LPAREN cond = expression RPAREN body = block_or_single {
      While (cond, body)
    }
  | FOR LPAREN var = ID EQUAL init = expression SEMICOLON cond = expression
    SEMICOLON ID EQUAL incr = expression RPAREN body = block_or_single {
      For (make_located var $startpos(var), init, cond, incr, body)
    }
  | RETURN e = expression? SEMICOLON { Return (e) }
  | PRINTF LPAREN s = STRING RPAREN SEMICOLON { Printf s }

else_clause:
  | ELSE s = block_or_single { s }

block_or_single:
  | LBRACE stmts = list(statement) RBRACE { stmts }
  | s = statement { [s] }

expression:
  | l = literal { Literal l }
  | id = ID { Identifier (make_located id $startpos(id)) } 
  | e1 = expression op = binop e2 = expression {
      BinaryOp (e1, make_located op $startpos(op), e2)
    }
  | op = unop e = expression { 
      UnaryOp (make_located op $startpos(op), e)
    }
  | func = ID LPAREN args = separated_list(COMMA, expression) RPAREN {
      FunctionCall (make_located func $startpos(func), args)  
    }
  | LPAREN e = expression RPAREN { e }

literal:
  | f = FLOAT { make_located (Float f) $startpos(f) }
  | s = STRING { make_located (String s) $startpos(s) }
  | TRUE { make_located (Bool true) $startpos }
  | FALSE { make_located (Bool false) $startpos }
  | NULL { make_located Null $startpos }

%inline binop:
  | PLUS { Add }
  | MINUS { Sub }
  | STAR { Mul }
  | SLASH { Div }
  | EQUAL_EQUAL { Eq }
  | BANG_EQUAL { Ne }
  | LESS { Lt }
  | LESS_EQUAL { Le }
  | GREATER { Gt }
  | GREATER_EQUAL { Ge }
  | AND { And }
  | OR { Or }

%inline unop:
  | BANG { Not }
  | MINUS { Neg }