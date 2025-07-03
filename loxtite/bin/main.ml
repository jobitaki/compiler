open Loxtite

let print_token = function
  | Parser.ID s -> Printf.printf "ID(%s)\n" s
  | Parser.INT i -> Printf.printf "INT(%d)\n" i
  | Parser.STRING s -> Printf.printf "STRING(%s)\n" s
  | Parser.LPAREN -> Printf.printf "LPAREN\n"
  | Parser.RPAREN -> Printf.printf "RPAREN\n"
  | Parser.EQUAL -> Printf.printf "EQUAL\n"
  | Parser.EOF -> Printf.printf "EOF\n"
  | _ -> Printf.printf "OTHER_TOKEN\n"

let rec tokenize_all lexbuf =
  match Lexer.read_token lexbuf with
  | Parser.EOF -> print_token Parser.EOF
  | token -> 
      print_token token;
      tokenize_all lexbuf

let () =
  let input = 
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else "let x = 42"
  in
  let lexbuf = Lexing.from_string input in
  Printf.printf "Tokenizing: %s\n" input;
  Printf.printf "Tokens:\n";
  tokenize_all lexbuf