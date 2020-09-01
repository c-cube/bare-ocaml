
{
open Bare_parser
}

let printable_char = [^ '\n']
let comment_line = '#' printable_char*

let numeric = ['0' - '9']
let lower_alpha = ['a' - 'z']
let upper_alpha = ['A' - 'Z']
let alpha_numeric = lower_alpha | upper_alpha | numeric | '_'

let ident = (upper_alpha | lower_alpha) alpha_numeric*

let int = ['0'-'9']+

rule token = parse
  | eof { EOF }
  | comment_line { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r'] { token lexbuf }
  | '(' { LEFT_PAREN }
  | ')' { RIGHT_PAREN }
  | '[' { LEFT_BRACKET }
  | ']' { RIGHT_BRACKET }
  | '{' { LEFT_BRACE }
  | '}' { RIGHT_BRACE }
  | '<' { LEFT_ANGLE }
  | '>' { RIGHT_ANGLE }
  | ':' { COLON }
  | '|' { PIPE }
  | '=' { EQUAL }
  | "type" { TYPE }
  | "data" { DATA }
  | "enum" { ENUM }
  | "map" { MAP }
  | "optional" { OPTIONAL }
  | int { INT(Lexing.lexeme lexbuf) }
  | ident { IDENT(Lexing.lexeme lexbuf) }
  | _ as c
    {
      let loc = Ast.P.loc_of_lexbuf lexbuf in
      Ast.P.errorf ~loc "invalid char '%c'" c
    }
