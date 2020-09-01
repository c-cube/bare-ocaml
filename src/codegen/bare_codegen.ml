
module A = Ast
module P = Bare_parser
module L = Bare_lexer

let spf = Printf.sprintf

let debug = ref false

module CG : sig
  type t
  val create : unit -> t
  val add_prelude : t -> unit

  val encode_ty_def : t -> A.ty_def -> unit

  val code : t -> string
  val write_code : out_channel -> t -> unit
end = struct
  type fmt = Format.formatter
  type t = {
    buf: Buffer.t;
    out: fmt;
  }

  let fpf (self:fmt) fmt = Format.fprintf self fmt
  let addstr self = Format.pp_print_string self

  let create () : t =
    let buf = Buffer.create 1024 in
    let out = Format.formatter_of_buffer buf in
    {out; buf}

  let add_prelude self =
    fpf self.out "[@@@@@@ocaml.warning \"-8-26-27\"]@.";
    ()

  let code self = fpf self.out "@."; Buffer.contents self.buf
  let write_code oc self = fpf self.out "@."; Buffer.output_buffer oc self.buf

  (* codegen type definition *)
  let rec cg_ty_ty ~root (self:fmt) (ty:A.ty_expr) : unit =
    let recurse = cg_ty_ty ~root:false in
    match ty with
    | A.Named_ty s -> fpf self "%s.t" (String.capitalize_ascii s)
    | A.Uint | A.Int -> addstr self "int64"
    | A.U8 | A.I8 -> addstr self "char"
    | A.U16 | A.I16 -> addstr self "int"
    | A.U32 | A.I32 -> addstr self "int32"
    | A.U64 | A.I64 -> addstr self "int64"
    | A.F32 | A.F64 -> addstr self "float"
    | A.Bool -> addstr self "bool"
    | A.String -> addstr self "string"
    | A.Data _ -> addstr self "bytes"
    | A.Void -> addstr self "unit"
    | A.Optional ty -> fpf self "%a option" recurse ty
    | A.Array {ty; len=_} -> fpf self "@[%a@ array@]" recurse ty
    | A.Map (String, b) -> fpf self "@[%a@ Bare.String_map.t@]" recurse b
    | A.Map (a, b) -> fpf self "@[(@[%a *@ %a@]) list@]" recurse a recurse b
    | A.Struct l ->
      assert root; (* flattened *)
      fpf self (if List.length l <= 2 then "@, @[<hv>{@[" else "@, @[<v>{@[<v>");
      List.iteri
        (fun i (name,ty) ->
           if i>0 then fpf self ";@ ";
           fpf self "%s: %a" name recurse ty)
        l;
      fpf self "@]@,}@]"

  (* named for the i-th element of an union *)
  let union_elt_name ~ty_name i (ty:A.ty_expr) : string =
    match ty with
    | Named_ty s -> String.capitalize_ascii s
    | _ -> spf "%s_%d" (String.capitalize_ascii ty_name) i

  (* for [enum name l], produce int64<->t conversions *)
  let cg_enum_conv self _name l =
    fpf self "@,@[<hv2>let to_int = function@,";
    begin
      let n = ref 0 in
      List.iter
        (function
          | (name,None) ->
            fpf self "| @[%s ->@ %dL@]@," (String.capitalize_ascii name) !n;
            incr n
          | (name,Some i) ->
            assert (i>= !n);
            fpf self "| @[%s -> %dL@]@," (String.capitalize_ascii name) i;
            n := i + 1;
        )
        l;
      fpf self "@]";
    end;
    fpf self "@,@[<hv2>let of_int = function@,";
    begin
      let n = ref 0 in
      List.iter
        (function
          | (name,None) ->
            fpf self "| @[%dL ->@ %s@]@," !n (String.capitalize_ascii name) ;
            incr n
          | (name,Some i) ->
            assert (i>= !n);
            fpf self "| @[%dL ->@ %s@]@," i (String.capitalize_ascii name);
            n := i + 1;
        )
        l;
      fpf self "| @[x -> failwith@ \
                (Printf.sprintf \"unknown enum member for %s.t: %%Ld\" x)@]@]@,"
        (String.capitalize_ascii _name);
    end;
    ()

  (* codegen for type definition of this type def *)
  let cg_ty_def_rhs_def ty_name self (tyd:A.ty_def_rhs) : unit =
    match tyd with
    | A.Atomic ty ->
      fpf self "@[<2>type t =@ %a@]@," (cg_ty_ty ~root:true) ty
    | A.Enum l ->
      fpf self "@[<hv2>type t =@ ";
      List.iteri
        (fun i (n,_) ->
           if i>0 then fpf self "@ | ";
           addstr self (String.capitalize_ascii n))
        l;
      fpf self "@]@ "
    | A.Union l ->
      fpf self "@[<v2>type t =@ ";
      List.iteri
        (fun i ty ->
           let name = union_elt_name ~ty_name i ty in
           match ty with
           | Void -> fpf self "| %s@ " name
           | _ -> fpf self "| @[%s of %a@]@ " name (cg_ty_ty ~root:false) ty)
        l;
      fpf self "@]@,"

  (* codegen for decoding *)
  let rec cg_ty_decode ~root ~ty_name (self:fmt) (ty:A.ty_expr) : unit =
    let recurse = cg_ty_decode ~root:false ~ty_name in
    match ty with
    | A.Named_ty s -> fpf self "%s.decode dec" (String.capitalize_ascii s)
    | A.Uint -> addstr self "Bare.Decode.uint dec"
    | A.Int -> addstr self "Bare.Decode.int dec"
    | A.U8 -> addstr self "Bare.Decode.i8 dec"
    | A.I8 -> addstr self "Bare.Decode.u8 dec"
    | A.U16 -> addstr self "Bare.Decode.u16 ec"
    | A.I16 -> addstr self "Bare.Decode.i16 dec"
    | A.U32 -> addstr self "Bare.Decode.u32 ec"
    | A.I32 -> addstr self "Bare.Decode.i32 dec"
    | A.U64 -> addstr self "Bare.Decode.u64 ec"
    | A.I64 -> addstr self "Bare.Decode.i64 dec"
    | A.F32 -> addstr self "Bare.Decode.f32 dec"
    | A.F64 -> addstr self "Bare.Decode.f64 dec"
    | A.Bool -> addstr self "Bare.Decode.bool dec"
    | A.String -> addstr self "Bare.Decode.string dec"
    | A.Data {len=None} -> addstr self "Bare.Decode.data dec"
    | A.Data {len=Some n} -> fpf self "Bare.Decode.data_of ~size:%d dec" n
    | A.Void -> addstr self "()"
    | A.Optional ty ->
      fpf self "@[<2>Bare.Decode.optional@ (@[fun dec ->@ %a@]) dec@]" recurse ty
    | A.Array {ty; len=Some len} ->
      fpf self "@[<2>Array.init %d@ (@[fun _ ->@ %a@])@]" len recurse ty
    | A.Array {ty; len=None} ->
      fpf self "(@[<v>let len = Bare.Decode.uint dec in@ \
                 if len>Int64.of_int Sys.max_array_length then failwith \"array too big\";@ \
                @[<2>Array.init (Int64.to_int len)@ (@[fun _ -> %a@])@]@])" recurse ty
    | A.Map (String, b) ->
      fpf self "(@[<v>let len = Bare.Decode.uint dec in@ \
                 if len>Int64.of_int Sys.max_array_length  then failwith \"array too big\";@ \
                 @[<2>List.init (Int64.to_int len)@ (@[<v>fun _ ->@ \
                let k = Bare.Decode.string dec in@ let v = %a in@ k,v@])@]@ \
                |> List.to_seq |> Bare.String_map.of_seq@])" recurse b
    | A.Map (a, b) ->
      fpf self "(@[<v>let len = Bare.Decode.uint dec in@ \
                if len>Int64.of_int Sys.max_array_length  then failwith \"array too big\";@ \
                 @[<2>List.init (Int64.to_int len)@ (@[fun _ ->@ \
                let k = %a in@ let v = %a@ in k,v@])@]@])" recurse a recurse b
    | A.Struct l ->
      assert root; (* flattened *)
      fpf self "@[<hv>";
      List.iter
        (fun (n,ty) -> fpf self "@[<2>let %s =@ %a in@]@ " n recurse ty)
        l;
      fpf self "{@[<hv>";
      List.iter (fun (n,_) -> fpf self "%s;@ "n ) l;
      fpf self "@]}@]"

  (* codegen for decoding *)
  let cg_ty_def_rhs_decode ty_name (self:fmt) (def:A.ty_def_rhs) : unit =
    match def with
    | A.Atomic ty -> cg_ty_decode ~root:true ~ty_name self ty
    | A.Enum _ ->
      fpf self "of_int (Bare.Decode.uint dec)";
    | A.Union l ->
      fpf self "let tag = Bare.Decode.uint dec in@ match tag with@ ";
      List.iteri
        (fun i ty ->
           fpf self "| @[%dL ->@ %s (%a)@]@," i
             (union_elt_name ~ty_name i ty) (cg_ty_decode ~root:false ~ty_name) ty)
        l;
      fpf self "| @[_ -> failwith \
                (Printf.sprintf \"unknown union tag %s.t: %%Ld\" tag)@]@," ty_name

  (* codegen for encoding [x] into [enc] *)
  let rec cg_ty_encode (x:string) ~root ~ty_name (self:fmt) (ty:A.ty_expr) : unit =
    let recurse x = cg_ty_encode ~root:false ~ty_name x in
    match ty with
    | A.Named_ty s -> fpf self "%s.encode enc %s" (String.capitalize_ascii s) x
    | A.Uint -> fpf self "Bare.Encode.uint enc %s" x
    | A.Int -> fpf self "Bare.Encode.int enc %s" x
    | A.U8 -> fpf self "Bare.Encode.i8 enc %s" x
    | A.I8 -> fpf self "Bare.Encode.u8 enc %s" x
    | A.U16 -> fpf self "Bare.Encode.u16 enc %s" x
    | A.I16 -> fpf self "Bare.Encode.i16 enc %s" x
    | A.U32 -> fpf self "Bare.Encode.u32 enc %s" x
    | A.I32 -> fpf self "Bare.Encode.i32 enc %s" x
    | A.U64 -> fpf self "Bare.Encode.u64 enc %s" x
    | A.I64 -> fpf self "Bare.Encode.i64 enc %s" x
    | A.F32 -> fpf self "Bare.Encode.f32 enc %s" x
    | A.F64 -> fpf self "Bare.Encode.f64 enc %s" x
    | A.Bool -> fpf self "Bare.Encode.bool enc %s" x
    | A.String -> fpf self "Bare.Encode.string enc %s" x
    | A.Data {len=None} -> fpf self "Bare.Encode.data enc %s" x
    | A.Data {len=Some n} -> fpf self "Bare.Encode.data_of ~size:%d enc %s" n x
    | A.Void -> fpf self "()"
    | A.Optional ty ->
      fpf self "@[<2>Bare.Encode.optional@ (@[fun enc xopt ->@ %a@]) enc %s@]" (recurse "xopt") ty x
    | A.Array {ty; len=Some _len} ->
      fpf self "@[<2>Array.iter (@[fun xi ->@ %a@])@ %s@]" (recurse "xi") ty x
    | A.Array {ty; len=None} ->
      fpf self "(@[<v>let arr = %s in@ \
                Bare.Encode.uint enc (Int64.of_int (Array.length arr));@ \
                @[Array.iter (@[fun xi ->@ %a@])@ arr@]@])" x (recurse "xi") ty
    | A.Map (String, b) ->
      fpf self "(@[<v>let m = %s in@ \
                Bare.Encode.uint enc (Int64.of_int (Bare.String_map.cardinal m));@ \
                @[<2>Bare.String_map.iter@ (@[fun x y ->@ Bare.Encode.string enc x;@ %a@])@ %s@]@])"
        x (recurse "y") b x
    | A.Map (a, b) ->
      fpf self "(@[<v>Bare.Encode.uint enc (Int64.of_int (List.length %s));
                @[<2>List.iter@ (@[fun (x,y) ->@ %a;@ %a@])@ %s@]@])"
        x (recurse "x") a (recurse "y") b x
    | A.Struct l ->
      assert root; (* flattened *)
      fpf self "@[<hv2>begin@ ";
      List.iteri
        (fun i (n,ty) ->
           if i>0 then fpf self "@ ";
           let field = spf "%s.%s" x n in
           fpf self "%a;" (recurse field) ty)
        l;
      fpf self "@;<1 -2>end@]";
      ()


  (* codegen for encoding *)
  let cg_ty_def_rhs_encode ty_name (self:fmt) (def:A.ty_def_rhs) : unit =
    match def with
    | A.Atomic ty -> cg_ty_encode "self" ~root:true ~ty_name self ty
    | A.Enum _ ->
      fpf self "Bare.Encode.uint enc (to_int self)";
    | A.Union l ->
      fpf self "@[<hv>match self with@ ";
      List.iteri
        (fun i ty ->
           fpf self "| @[<v>%s x ->@ \
                     Bare.Encode.uint enc %dL;@ \
                     %a@]@,"
             (union_elt_name ~ty_name i ty) i
             (cg_ty_encode ~root:false ~ty_name "x") ty)
        l

  (* in the scope of a module, define type and functions *)
  let cg_ty_def_in_mod name self def =
    cg_ty_def_rhs_def name self def;
    begin match def with
      | Enum l -> cg_enum_conv self name l
      | _ -> ()
    end;
    fpf self "@,(** @raise Bare.Decode.Error in case of error. *)@,\
              @[<2>let decode (dec: Bare.Decode.t) : t =@ %a@]@,"
      (cg_ty_def_rhs_decode name) def;
    fpf self "@,@[<2>let encode (enc: Bare.Encode.t) (self: t) : unit =@ %a@]@,"
      (cg_ty_def_rhs_encode name) def;
    ()

  let encode_ty_def (self:t) (d:A.ty_def) : unit =
    let {A.name; def} = d in
    fpf self.out "@[<v2>module %s = struct@," (String.capitalize_ascii name);
    fpf self.out "%a" (cg_ty_def_in_mod name) def;
    fpf self.out "@]@.end@.@.";
    ()

end

let parse_file f : A.ty_def list =
  if !debug then Printf.eprintf "parse file %S\n%!" f;
  let ic = open_in f in
  let lexbuf = Lexing.from_channel ~with_positions:true ic in
  try
    let l = Bare_parser.top_defs Bare_lexer.token lexbuf in
    close_in ic;
    l
  with
  | Parsing.Parse_error ->
    let loc = A.P.loc_of_lexbuf lexbuf in
    Format.eprintf "parse error at %a@." A.P.pp_loc loc;
    exit 1
  | e ->
    close_in_noerr ic;
    raise e

let normalize_defs defs =
  A.flatten_types defs

let codegen ~out defs : unit =
  let cg = CG.create() in
  CG.add_prelude cg;
  List.iter (CG.encode_ty_def cg) defs;
  if !debug then Printf.eprintf "generate code into %S\n" out;
  let oc = open_out out in
  CG.write_code oc cg;
  flush oc;
  close_out oc;
  ()

let () =
  let files = ref [] in
  let cat = ref false in
  let out = ref "" in
  let opts = [
    "--cat", Arg.Set cat, " print type definitions";
    "-d", Arg.Set debug, " debug mode";
    "-o", Arg.Set_string out, " set output file for codegen";
  ] |> Arg.align in
  Arg.parse opts (fun f -> files := f :: !files) "usage: bare-codegen [opt]* file+";
  let tys = List.map parse_file (List.rev !files) |> List.flatten in
  if !cat then (
    List.iter (fun td -> Format.printf "%a@.@." A.pp_ty_def td) tys;
  );
  let tys = normalize_defs tys in
  if !out <> "" then (
    codegen ~out:!out tys
  );
  ()

