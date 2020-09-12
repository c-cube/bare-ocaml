
let debug = ref false

let parse_file f : Ast.ty_def list =
  if !debug then Printf.eprintf "parse file %S\n%!" f;
  let ic = open_in f in
  let lexbuf = Lexing.from_channel ~with_positions:true ic in
  try
    let l = Bare_parser.top_defs Bare_lexer.token lexbuf in
    close_in ic;
    l
  with
  | Parsing.Parse_error ->
    let loc = Ast.P.loc_of_lexbuf lexbuf in
    Format.eprintf "parse error at %a@." Ast.P.pp_loc loc;
    exit 1
  | e ->
    close_in_noerr ic;
    raise e

type lang =
  | ML
  | C

let parse_lang str =
  match str with
  | "ml" -> ML
  | "c" -> C
  | lang -> raise (Invalid_argument ("Unknown lang value: " ^ lang))


let lang_to_string str =
  match str with
  | ML -> "ml"
  | C -> "c"

let () =
  let files = ref [] in
  let cat = ref false in
  let out = ref "" in
  let stdout = ref false in
  let lang = ref ML in
  let opts = [
    "--cat", Arg.Set cat, " print type definitions";
    "-d", Arg.Set debug, " debug mode";
    "-o", Arg.Set_string out, " codegen: print code to given file";
    "-l", Arg.String (fun str -> lang := parse_lang str), " lang: ml|c (default=ml)";
    "--stdout", Arg.Set stdout, " codegen: print code to stdout"
  ] |> Arg.align in
  Arg.parse opts (fun f -> files := f :: !files) "usage: bare-codegen [opt]* file+";
  print_endline (">>> Generating " ^ lang_to_string !lang);
  let tys = List.map parse_file (List.rev !files) |> List.flatten in

  if !cat then (
    List.iter (fun td -> Format.printf "%a@.@." Ast.pp_ty_def td) tys;
  );
  if !stdout || !out <> "" then (
    match !lang with
    | ML -> Codegen_ocaml.codegen ~to_stdout:!stdout ~out:!out tys
    | C  -> Codegen_c.codegen     ~to_stdout:!stdout ~out:!out tys
  );
  ()
