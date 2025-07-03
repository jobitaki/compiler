{
open Lexing
open Parser

exception SyntaxError of string

(* Function that updates the lexbuf current position
   with the next line *)
let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
        { pos with pos_bol = lexbuf.lex_curr_pos;
                   pos_lnum = pos.pos_lnum + 1
        }
}

(* Define helper regexes *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let int = '-'? digit+ (* regex for integers *)
let id = (alpha) (alpha|digit|'_')* (* regex for identifiers *)
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read_token = 
    parse
    | "(" { LPAREN }
    | ")" { RPAREN }
    | "{" { LBRACE }
    | "}" { RBRACE }
    | "+" { PLUS }
    | "-" { MINUS }
    | "/" { SLASH }
    | "*" { SLASH }
    | "," { COMMA }
    | "." { DOT }
    | ";" { SEMICOLON }
    | "!" { BANG }
    | "=" { EQUAL }
    | ">" { GREATER }
    | "<" { LESS }
    | "!=" { BANG_EQUAL }
    | ">=" { GREATER_EQUAL }
    | "<=" { LESS_EQUAL }
    | "var" { VAR }
    | "class" { CLASS }
    | "and" { AND }
    | "or" { OR }
    | "if" { IF }
    | "else" { ELSE }
    | "for" { FOR }
    | "while" { WHILE }
    | "fun" { FUN }
    | "return" { RETURN }
    | "nil" { NIL }
    | "printf" { PRINTF }
    (* We recursively call read_token because we don't return anything 
       meaningful. We recursively call the same rule again when we want to
       continue.
    *)
    | whitespace { read_token lexbuf }
    | "//" { read_single_line_comment lexbuf }
    | "/*" { read_multi_line_comment lexbuf }
    (* Lexing.lexeme lexbuf returns the string read *)
    | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | id { ID (Lexing.lexeme lexbuf) }
    (* When string detected, trigger string rule with a new buffer *)
    | '"' {read_string (Buffer.create 17) lexbuf }
    | newline { next_line lexbuf; read_token lexbuf }
    | eof { EOF }
    | _ { raise (SyntaxError ("Lexer - Illegal character: " ^ Lexing.lexeme lexbuf)) }

and read_single_line_comment = parse
    | newline { next_line lexbuf; read_token lexbuf }
    | eof { EOF }
    | _ { read_single_line_comment lexbuf }

and read_multi_line_comment = parse
    | "*/" { read_token lexbuf }
    | newline { next_line lexbuf; read_multi_line_comment lexbuf }
    | eof { raise (SyntaxError ("Lexer - Unexpected EOF - terminate comment.")) }
    | _ { read_multi_line_comment lexbuf }

and read_string buf = parse
    | '"' { STRING (Buffer.contents buf) }
    (* When encountering a newline as an escaped char, add it to buf and keep
       going.
    *)
    | "\\" "n" { Buffer.add_char buf '\n'; read_string buf lexbuf }
    (* TODO add more regexes to handle other characters *)
    | [^ '"' '\\'] 
        { Buffer.add_string buf (Lexing.lexeme lexbuf);
          read_string buf lexbuf
        }
    | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
    | eof { raise (SyntaxError ("String is not terminated"))}
