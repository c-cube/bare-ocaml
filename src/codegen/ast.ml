
type pos = Lexing.position
type loc = {start: pos; end_: pos}

(** {2 Main AST} *)

type ty_expr =
  | Named_ty of string
  | Uint
  | Int
  | U8
  | U16
  | U32
  | U64
  | I8
  | I16
  | I32
  | I64
  | F32
  | F64
  | Bool
  | String
  | Data of { len: int option }
  | Void
  | Optional of ty_expr
  | Array of {
      ty: ty_expr;
      len: int option;
    }
  | Map of ty_expr * ty_expr
  | Struct of (string * ty_expr) list

type ty_def_rhs =
  | Atomic of ty_expr
  | Enum of (string * int option) list
  | Union of ty_expr list

type ty_def = {
  name: string;
  def: ty_def_rhs;
(*  TODO:  pos: pos; *)
}

(** remove struct constructs that occur inside types, declaring them at
    toplevel *)
let flatten_types (defs: ty_def list) : ty_def list =
  let cnt = ref 0 in
  let new_defs = ref [] in
  let addef d = new_defs := d :: !new_defs in
  let rec aux_ty ~root path ty =
    let recurse ?(path=path) ty = aux_ty path ~root:false ty in
    match ty with
    | Array {ty;len} -> Array {len;ty=recurse ty}
    | Map (a,b) -> Map(recurse a, recurse b)
    | Struct l when root ->
      Struct (List.map (fun (n,ty) -> n, recurse ~path:(n::path) ty)l )
    | Struct l ->
      (* introduce a name for this sub-struct *)
      let new_name =
        let path = String.concat "_" @@ List.rev path in
        Printf.sprintf "%s_%d" path !cnt in
      incr cnt;
      begin
        (* side definition for this new type *)
        let new_def = {name=new_name; def=Atomic (Struct l)} in
        aux_def new_def;
      end;
      Named_ty new_name
    | Optional ty -> Optional (recurse ty)
    | Named_ty _ | Uint | Int | U8 | U16 | U32 | U64 | I8 | I16 | I32 | I64
    | F32 | F64 | Bool | String | Void | Data _ -> ty

  and aux_def ({name;def} as td) =
    let def = match def with
      | Enum _ -> def
      | Atomic ty -> Atomic (aux_ty ~root:true [name] ty)
      | Union l -> Union (List.map (aux_ty ~root:false [name]) l)
    in
    addef {td with def};
  in
  List.iter aux_def defs;
  List.rev !new_defs

(** {2 Printers} *)

let ppstr = Format.pp_print_string
let fpf = Format.fprintf
let pplist sep ppx out l =
  Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out " %s@ " sep) ppx out l

let rec pp_ty_expr out = function
  | Uint -> ppstr out "uint"
  | Int -> ppstr out "int"
  | U8 -> ppstr out "u8"
  | U16 -> ppstr out "u16"
  | U32 -> ppstr out "u32"
  | U64 -> ppstr out "u64"
  | I8 -> ppstr out "i8"
  | I16 -> ppstr out "i16"
  | I32 -> ppstr out "i32"
  | I64 -> ppstr out "i64"
  | F32 -> ppstr out "f32"
  | F64 -> ppstr out "f64"
  | Bool -> ppstr out "bool"
  | Void -> ppstr out "void"
  | String -> ppstr out "string"
  | Data {len=None} -> ppstr out "data"
  | Data {len=Some n} -> fpf out "data<%d>" n
  | Named_ty s -> ppstr out s
  | Optional ty -> fpf out "optional<@[%a@]>" pp_ty_expr ty
  | Array {ty; len=None} ->
    fpf out "@[[]@[%a@]@]" pp_ty_expr ty
  | Array {ty; len=Some n} ->
    fpf out "@[[%d]@[%a@]@]" n pp_ty_expr ty
  | Map (a,b) ->
    fpf out "@[map[@[%a@]]@[%a@]@]" pp_ty_expr a pp_ty_expr b
  | Struct l ->
    let pppair out (n,ty) = fpf out "@[<1>%s:@ %a@]" n pp_ty_expr ty in
    fpf out "{@[<hv>%a@]}" (pplist "" pppair) l

let pp_ty_def out (td:ty_def) : unit =
  let {name; def} = td in
  match def with
  | Atomic ty -> fpf out "@[<2>type %s@ %a@]" name pp_ty_expr ty
  | Enum l ->
    let ppc out = function
      | (s,None) -> ppstr out s
      | (s,Some i) -> fpf out "%s = %d" s i
    in
    fpf out "@[<2>enum %s@ {@[<v>%a@]}@]" name (pplist "" ppc) l
  | Union l ->
    fpf out "@[<2>type %s@ (@[%a@])@]" name (pplist "|" pp_ty_expr) l

(** {2 Parser utils} *)
module P = struct
  let ty_expr_l = [
    "uint", Uint;
    "int", Int;
    "u8", U8;
    "u16", U16;
    "u32", U32;
    "u64", U64;
    "i8", I8;
    "i16", I16;
    "i32", I32;
    "i64", I64;
    "f32", F32;
    "f64", F64;
    "bool", Bool;
    "void", Void;
    "string", String;
  ]

  exception Parse_error of string * loc

  let loc_of_lexbuf lexbuf =
    let start = lexbuf.Lexing.lex_start_p in
    let end_ = lexbuf.Lexing.lex_curr_p in
    { start; end_ }

  let errorf ~loc fmt =
    Format.kasprintf (fun s -> raise (Parse_error (s,loc))) fmt

  let pp_loc out loc =
    let col x = x.Lexing.pos_cnum - x.Lexing.pos_bol in
    let line x = x.Lexing.pos_lnum in
    Format.fprintf out "%d:%d to %d:%d"
      (line loc.start) (col loc.start)
      (line loc.end_) (col loc.end_)

end
