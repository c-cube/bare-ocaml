%{
  open Ast
  open Ast.P
%}

%token EOF

%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_ANGLE
%token RIGHT_ANGLE

%token TYPE
%token COMMA
%token COLON
%token EQUAL
%token PIPE

%token STRUCT
%token UNION
%token ENUM
%token LIST
%token OPTIONAL
%token MAP
%token DATA

%token <string> IDENT
%token <string> INT

%type <Ast.ty_def> top_def
%type <Ast.ty_def list> top_defs
%type <Ast.ty_expr> top_ty

%start top_def
%start top_defs
%start top_ty

%%

top_def: ty_def EOF { $1 }
top_ty: ty EOF { $1 }

top_defs:
  | EOF { [] }
  | ty_def top_defs { $1 :: $2 }


ty:
  | DATA { Data {len=None} }
  | DATA LEFT_BRACKET INT RIGHT_BRACKET { Data {len=Some (int_of_string $3)}  }
  | IDENT {
    try List.assoc $1 ty_expr_l
    with Not_found -> Ast.Named_ty {name=$1; is_void=false}
  }
  | OPTIONAL LEFT_ANGLE ty RIGHT_ANGLE { Optional $3 }
  | LIST LEFT_ANGLE ty RIGHT_ANGLE { Array {ty=$3; len=None} }
  | ty LEFT_BRACKET INT RIGHT_BRACKET { Array {ty=$1; len=Some (int_of_string $3)} }
  | MAP LEFT_ANGLE ty RIGHT_ANGLE LEFT_ANGLE ty RIGHT_ANGLE { Map ($3, $6) }
  | STRUCT LEFT_BRACE struct_fields RIGHT_BRACE { Struct $3 }

ty_def:
  | TYPE IDENT UNION LEFT_BRACE union_items RIGHT_BRACE {
    {Ast. name=$2; def=Union $5}
  }
  | TYPE IDENT ty {
(*     let loc = Ast.loc_of_lexbuf lexbuf in *)
    {Ast. name=$2; def=Atomic $3}
  }
  | TYPE IDENT ENUM LEFT_BRACE enum_items RIGHT_BRACE {
(*     let loc = Ast.loc_of_lexbuf lexbuf in *)
    {Ast. name=$2; def=Enum $5}
  }

struct_fields:
  | { [] }
  | IDENT COLON ty struct_fields { ($1, $3) :: $4 }

enum_item:
  | IDENT { $1,None }
  | IDENT EQUAL INT { $1, Some (int_of_string $3) }

enum_items:
  | { [] }
  | enum_item { [$1] }
  | enum_item enum_items { $1 :: $2 }

union_items:
  | { [] }
  | ty { [$1] }
  | ty PIPE union_items { $1 :: $3 }

%%
