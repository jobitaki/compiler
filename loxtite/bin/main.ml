open Loxtite

let usage_msg = "my_compiler [options] <file>"
let input_file = ref ""
let debug_mode = ref false

let set_input_file filename = input_file := filename

let spec_list = [
  ("-debug", Arg.Set debug_mode, "Enable debug mode");
  ("-ast", Arg.Set debug_mode, "Print AST (same as -debug for now)");
]

let () =
  Arg.parse spec_list set_input_file usage_msg;
  
  if !input_file = "" then begin
    Printf.printf "Usage: %s\n" usage_msg;
    Printf.printf "Options:\n";
    Printf.printf "  -debug    Enable debug mode\n";
    Printf.printf "  -ast      Print AST\n";
    exit 1
  end;

  if !debug_mode then
    Printf.printf "Compiling file: %s\n" !input_file;

  Compiler.compile_file !input_file