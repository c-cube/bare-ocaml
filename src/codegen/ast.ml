
let spf = Printf.sprintf
type pos = Lexing.position
type loc = {start: pos; end_: pos}

(** {2 Main AST} *)

type ty_expr =
  | Named_ty of {
      name: string;
      is_void: bool;
    }
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

let iter_ty f (ty:ty_expr) : unit =
  let rec aux ty =
    f ty;
    match ty with
    | Array {ty;len=_ } -> aux ty
    | Map (a,b) -> aux a; aux b
    | Struct l -> List.iter (fun (_,u) -> aux u) l
    | Optional ty -> aux ty
    | Named_ty _ | Uint | Int | U8 | U16 | U32 | U64 | I8 | I16 | I32 | I64
    | F32 | F64 | Bool | String | Void | Data _ -> ()
  in
  aux ty

module Find_scc : sig
  val top: ty_def list -> ty_def list list
  (** Group definitions into clusters of mutually recursive definitions. *)
end = struct
  (* code reused from containers to compute strongly connected components *)

  type name = string

  type state = {
    mutable min_id: int; (* min ID of the vertex' scc *)
    id: int;  (* ID of the vertex *)
    mutable on_stack: bool;
    mutable vertex: ty_def;
  }

  let mk_cell (d:ty_def) n = {
    min_id=n;
    id=n;
    on_stack=false;
    vertex=d;
  }

  (* pop elements of [stack] until we reach node with given [id] *)
  let rec pop_down_to ~id acc stack =
    assert (not(Stack.is_empty stack));
    let cell = Stack.pop stack in
    cell.on_stack <- false;
    if cell.id = id then (
      assert (cell.id = cell.min_id);
      cell.vertex :: acc (* return SCC *)
    ) else (
      pop_down_to ~id (cell.vertex::acc) stack
    )

  let iter_def_rhs f = function
    | Atomic ty -> iter_ty f ty
    | Union l -> List.iter (iter_ty f) l
    | Enum _ -> ()

  let iter_out_edges ~graph f (d:ty_def) =
    iter_def_rhs
      (function
        | Named_ty n' ->
          let d' =
            try Hashtbl.find graph n'.name
            with Not_found -> failwith (spf "type %s not found" n'.name)
          in
          f d'
        | _ -> ())
      d.def

  let top (l:ty_def list) : _ list list =
    (* turn [l] into a map for easy access *)
    let graph =
      List.map (fun d -> d.name, d) l
      |> List.to_seq |> Hashtbl.of_seq
    in
    let tbl: (name, state) Hashtbl.t = Hashtbl.create 32 in
    let scc_l : _ list list ref = ref [] in
    begin
      (* stack of nodes being explored, for the DFS *)
      let to_explore = Stack.create() in
      (* stack for Tarjan's algorithm itself *)
      let stack = Stack.create () in
      (* unique ID *)
      let n = ref 0 in
      (* exploration *)
      List.iter
        (fun v ->
           Stack.push (`Enter v) to_explore;
           while not (Stack.is_empty to_explore) do
             match Stack.pop to_explore with
               | `Enter d ->
                 if not (Hashtbl.mem tbl d.name) then (
                   (* remember unique ID for [v] *)
                   let id = !n in
                   incr n;
                   let cell = mk_cell d id in
                   cell.on_stack <- true;
                   Hashtbl.add tbl d.name cell;
                   Stack.push cell stack;
                   Stack.push (`Exit (d, cell)) to_explore;
                   (* explore children *)
                   iter_out_edges ~graph
                     (fun d' -> Stack.push (`Enter d') to_explore)
                     d;
                 )
               | `Exit (d, cell) ->
                 (* update [min_id] *)
                 assert cell.on_stack;
                 iter_out_edges ~graph
                   (fun d' ->
                      (* must not fail, [dest] already explored *)
                      let dest_cell = Hashtbl.find tbl d'.name in
                      (* same SCC? yes if [dest] points to [cell.v] *)
                      if dest_cell.on_stack
                      then cell.min_id <- min cell.min_id dest_cell.min_id
                   )
                   d;
                 (* pop from stack if SCC found *)
                 if cell.id = cell.min_id then (
                   let scc = pop_down_to ~id:cell.id [] stack in
                   scc_l := scc :: !scc_l;
                 )
           done)
      l;
      assert (Stack.is_empty stack);
      ()
    end;
    List.rev !scc_l
end

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
      Named_ty {name=new_name; is_void=false}
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

(** replace `type a void` with `void` in every union it occurs in *)
let replace_void_defs (defs: ty_def list) : ty_def list =
  let void_types : (string, unit) Hashtbl.t = Hashtbl.create 8 in

  let find_void_type {name;def} =
    match def with
    | Atomic Void -> Hashtbl.add void_types name ();
    | _ -> ()
  in
  List.iter find_void_type defs;
  (* now replace `name` with `{name; is_void=true}` in all types *)
  let rec set_is_void_tag ty =
    let recurse = set_is_void_tag in
    match ty with
    | Named_ty {name;_} when Hashtbl.mem void_types name ->
      Named_ty {name; is_void=true}
    | Array {ty;len} -> Array {len;ty=recurse ty}
    | Map (a,b) -> Map(recurse a, recurse b)
    | Struct l -> Struct (List.map (fun (n,ty) -> n, recurse ty) l)
    | Optional ty -> Optional (recurse ty)
    | Named_ty _ | Uint | Int | U8 | U16 | U32 | U64 | I8 | I16 | I32 | I64
    | F32 | F64 | Bool | String | Void | Data _ -> ty
  in
  let aux_def ({def;_} as td) =
    begin match def with
      | Atomic Void -> td
      | Enum _ -> td
      | Atomic ty ->
        {td with def=Atomic (set_is_void_tag ty)}
      | Union l ->
        let l =
          List.map
            (function
              | Named_ty n when Hashtbl.mem void_types n.name ->
                Named_ty {n with is_void=true} (* will be a nullary cstor *)
              | ty -> set_is_void_tag ty)
            l
        in
        {td with def=Union l}
    end
  in
  List.map aux_def defs

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
  | Named_ty {name;_} -> ppstr out name
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
