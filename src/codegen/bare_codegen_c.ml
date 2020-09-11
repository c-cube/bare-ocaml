
let codegen ~to_stdout ~out defs : unit =
  ignore (to_stdout, out, defs);
  print_endline "HELLO"
