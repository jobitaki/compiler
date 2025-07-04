open Ast
open Lexing

exception LexError of string * pos
exception ParseError of string * pos
exception SemanticError of string * pos

type compile_result =
  | Success of program
  | Error of string

let parse_string (input: string) : compile_result =
  let lexbuf = Lexing.from_string input in
  try
    let ast = Parser.program Lexer.read_token lexbuf in
    Success ast
  with
  | Lexer.SyntaxError msg ->
      let pos = { line = lexbuf.lex_curr_p.pos_lnum; 
                 column = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol } in
      Error (Printf.sprintf "Lexer error at line %d, column %d: %s" 
             pos.line pos.column msg)
  | Parser.Error ->
      let pos = { line = lexbuf.lex_curr_p.pos_lnum; 
                 column = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol } in
      Error (Printf.sprintf "Parse error at line %d, column %d" pos.line pos.column)
  | Failure msg ->
      Error ("Parse error: " ^ msg)

let parse_file (filename: string) : compile_result = 
  try
    let ic = open_in filename in
    let input = really_input_string ic (in_channel_length ic) in
    close_in ic;
    parse_string input
  with
  | Sys_error msg -> Error ("File error: " ^ msg)

let print_ast (ast: program) : unit =
  Printf.printf "=== AST ===\n";
  Printf.printf "%s\n" (show_program ast);
  Printf.printf "==========\n"

(* Compile a program through all phases *)
let compile (input: string) : unit =
  match parse_string input with
  | Success ast -> 
      print_ast ast;
      (* TODO: Add semantic analysis *)
      (* TODO: Add MLIR generation *)  
      (* TODO: Add LLVM generation *)
      Printf.printf "Compilation successful!\n"
  | Error msg ->
      Printf.printf "Compilation failed: %s\n" msg

let compile_file (filename: string) : unit =
  match parse_file filename with
  | Success ast ->
      print_ast ast;
      Printf.printf "Compilation successful!\n"
  | Error msg ->
      Printf.printf "Compilation failed: %s\n" msg