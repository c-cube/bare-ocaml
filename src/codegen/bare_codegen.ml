
module A = Ast
module P = Bare_parser
module L = Bare_lexer

let spf = Printf.sprintf

let debug = ref false

module CG : sig
  type t
  val create : unit -> t

  val add_prelude : filenames:string list -> standalone:bool -> t -> unit
  (** @param standalone if true, add the runtime
      library into the code to get a standalone module *)

  val encode_ty_defs : t -> pp:bool -> A.ty_def list -> unit

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

  let add_prelude ~filenames ~standalone self =
    fpf self.out "(* generated from %s using bare-codegen *)@.\
                  [@@@@@@ocaml.warning \"-26-27\"]@."
      (String.concat "," @@ List.map (Printf.sprintf "%S") filenames);
    if standalone then (
      fpf self.out "@.(* embedded runtime library *)@.\
                    @[<v>@[<2>module Bare = struct@ %s@]@ end@]\
                    @.@." Embeded_lib.code;
    ) else (
      fpf self.out "module Bare = Bare_encoding@.";
    );
    ()

  let code self = fpf self.out "@."; Buffer.contents self.buf
  let write_code oc self = fpf self.out "@."; Buffer.output_buffer oc self.buf

  (* codegen type definition.
     root: is [ty] directly at the top of a definition
     clique: other types in the same mutually-recursive clique as [ty] *)
  let rec cg_ty_ty ~root ~clique (self:fmt) (ty:A.ty_expr) : unit =
    let recurse = cg_ty_ty ~root:false ~clique in
    match ty with
    | A.Named_ty {name;_} ->
      if List.mem name clique then (
        fpf self "%s" (String.uncapitalize_ascii name)
      ) else (
        fpf self "%s.t" (String.capitalize_ascii name)
      )
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
      fpf self "{@,";
      List.iteri
        (fun i (name,ty) ->
           if i>0 then fpf self "@ ";
           fpf self "%s: %a;" name recurse ty)
        l;
      fpf self "@;<0 -2>}"

  (* named for the i-th element of an union *)
  let union_elt_name ~ty_name i (ty:A.ty_expr) : string =
    match ty with
    | Named_ty {name;_} -> String.capitalize_ascii name
    | _ -> spf "%s_%d" (String.capitalize_ascii ty_name) i

  (* for [enum name l], produce int64<->t conversions *)
  let cg_enum_conv self _name l : unit =
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
      fpf self "| @[x -> invalid_arg@ \
                (Printf.sprintf \"unknown enum member for %s.t: %%Ld\" x)@]@]@,"
        (String.capitalize_ascii _name);
    end;
    ()

  (* codegen for type definition of this type def *)
  let cg_ty_def_rhs_def ~lead ~self_ty ~clique ty_name self (tyd:A.ty_def_rhs) : unit =
    match tyd with
    | A.Atomic ty ->
      fpf self "@[<v2>%s %s = %a@]@," lead self_ty (cg_ty_ty ~clique ~root:true) ty
    | A.Enum l ->
      fpf self "@[<hv2>%s %s =@ " lead self_ty;
      List.iteri
        (fun i (n,_) ->
           if i>0 then fpf self "@ | ";
           addstr self (String.capitalize_ascii n))
        l;
      fpf self "@]@ "
    | A.Union l ->
      fpf self "@[<v2>%s %s =@ " lead self_ty;
      List.iteri
        (fun i ty ->
           let name = union_elt_name ~ty_name i ty in
           match ty with
           | Named_ty{is_void=true;_} | Void -> fpf self "| %s@ " name
           | _ -> fpf self "| @[%s of %a@]@ " name (cg_ty_ty ~clique ~root:false) ty)
        l;
      fpf self "@]@,"

  (* codegen for decoding *)
  let rec cg_ty_decode ~root ~clique ~ty_name (self:fmt) (ty:A.ty_expr) : unit =
    let recurse = cg_ty_decode ~clique ~root:false ~ty_name in
    match ty with
    | A.Named_ty {name;is_void=true} ->
      (* refer to the constructor instead *)
      let cstor = union_elt_name ~ty_name:name (-1) ty in
      addstr self cstor
    | A.Named_ty {name;_} ->
      if List.mem name clique then (
        (* use the recursion callback *)
        fpf self "!_decode_%s dec" (String.uncapitalize_ascii name)
      ) else (
        fpf self "%s.decode dec" (String.capitalize_ascii name)
      )
    | A.Uint -> addstr self "Bare.Decode.uint dec"
    | A.Int -> addstr self "Bare.Decode.int dec"
    | A.U8 -> addstr self "Bare.Decode.i8 dec"
    | A.I8 -> addstr self "Bare.Decode.u8 dec"
    | A.U16 -> addstr self "Bare.Decode.u16 dec"
    | A.I16 -> addstr self "Bare.Decode.i16 dec"
    | A.U32 -> addstr self "Bare.Decode.u32 dec"
    | A.I32 -> addstr self "Bare.Decode.i32 dec"
    | A.U64 -> addstr self "Bare.Decode.u64 dec"
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
                if len>Int64.of_int Sys.max_array_length then \
                  invalid_arg \"array too big\";@ \
                @[<2>Array.init (Int64.to_int len)@ (@[fun _ -> %a@])@]@])" recurse ty
    | A.Map (String, b) ->
      fpf self "(@[<v>let len = Bare.Decode.uint dec in@ \
                if len>Int64.of_int max_int then \
                 invalid_arg \"array too big\";@ \
                 @[<2>List.init (Int64.to_int len)@ (@[<v>fun _ ->@ \
                let k = Bare.Decode.string dec in@ let v = %a in@ k,v@])@]@ \
                |> List.to_seq |> Bare.String_map.of_seq@])" recurse b
    | A.Map (a, b) ->
      fpf self "(@[<v>let len = Bare.Decode.uint dec in@ \
                if len>Int64.of_int Sys.max_array_length \
                then invalid_arg \"array too big\";@ \
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
  let cg_ty_def_rhs_decode ~clique ty_name (self:fmt) (def:A.ty_def_rhs) : unit =
    match def with
    | A.Atomic ty -> cg_ty_decode ~clique ~root:true ~ty_name self ty
    | A.Enum _ ->
      fpf self "of_int (Bare.Decode.uint dec)";
    | A.Union l ->
      fpf self "let tag = Bare.Decode.uint dec in@ match tag with@ ";
      List.iteri
        (fun i ty ->
           let cstor = union_elt_name ~ty_name i ty in
           match ty with
           | Named_ty {is_void=true;_} | Void ->
             (* nullary *)
             fpf self "| @[%dL ->@ %s@]@ " i cstor
           | _ ->
             fpf self "| @[%dL ->@ %s (%a)@]@ " i
               cstor (cg_ty_decode ~clique ~root:false ~ty_name) ty)
        l;
      fpf self "| @[_ -> invalid_arg@ \
                (Printf.sprintf \"unknown union tag %s.t: %%Ld\" tag)@]@," ty_name

  (* codegen for encoding [x] into [enc] *)
  let rec cg_ty_encode (x:string) ~clique ~root ~ty_name (self:fmt) (ty:A.ty_expr) : unit =
    let recurse x = cg_ty_encode ~clique ~root:false ~ty_name x in
    match ty with
    | A.Named_ty {name;_} ->
      if List.mem name clique then (
        (* use the recursion callback *)
        fpf self "!_encode_%s enc %s" (String.uncapitalize_ascii name) x
      ) else (
        fpf self "%s.encode enc %s" (String.capitalize_ascii name) x
      )
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
    | A.Data {len=Some n} ->
      fpf self "(@[assert (Bytes.length %s=%d);@ \
                 Bare.Encode.data_of ~size:%d enc %s@])" x n n x
    | A.Void -> fpf self "()"
    | A.Optional ty ->
      fpf self "@[<2>Bare.Encode.optional@ (@[fun enc xopt ->@ %a@]) enc %s@]" (recurse "xopt") ty x
    | A.Array {ty; len=Some len} ->
      fpf self "(@[<2>assert (Array.length %s = %d);@ \
                Array.iter (@[fun xi ->@ %a@])@ %s@])" x len (recurse "xi") ty x
    | A.Array {ty; len=None} ->
      fpf self "(@[<v>let arr = %s in@ \
                Bare.Encode.uint enc (Int64.of_int (Array.length arr));@ \
                @[Array.iter (@[fun xi ->@ %a@])@ arr@]@])" x (recurse "xi") ty
    | A.Map (String, b) ->
      fpf self "(@[<v>Bare.Encode.uint enc (Int64.of_int (Bare.String_map.cardinal %s));@ \
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
  let cg_ty_def_rhs_encode ty_name ~clique (self:fmt) (def:A.ty_def_rhs) : unit =
    match def with
    | A.Atomic ty -> cg_ty_encode "self" ~clique ~root:true ~ty_name self ty
    | A.Enum _ ->
      fpf self "Bare.Encode.uint enc (to_int self)";
    | A.Union l ->
      fpf self "@[<hv>match self with@ ";
      List.iteri
        (fun i ty ->
           let cstor = union_elt_name ~ty_name i ty in
           match ty with
           | A.Void | A.Named_ty {is_void=true;_} ->
             fpf self "| @[<v>%s ->@ \
                       Bare.Encode.uint enc %dL@]@," cstor i
           | _ ->
             fpf self "| @[<v>%s x ->@ \
                       Bare.Encode.uint enc %dL;@ \
                       %a@]@,"
               cstor i (cg_ty_encode ~clique ~root:false ~ty_name "x") ty)
        l

  (* define encoding/decoding/annex functions for [def] *)
  let cg_ty_encode_decode ~clique name self def : unit =
    begin match def with
      | A.Enum l -> cg_enum_conv self name l
      | _ -> ()
    end;
    fpf self "@,(** @raise Invalid_argument in case of error. *)@,\
              @[<v2>let decode (dec: Bare.Decode.t) : t =@ %a@]@,"
      (cg_ty_def_rhs_decode ~clique name) def;
    fpf self "@,@[<v2>let encode (enc: Bare.Encode.t) (self: t) : unit =@ %a@]@,"
      (cg_ty_def_rhs_encode ~clique name) def;
    ()

  (* codegen for a pretty printer [<ty> Bare.Pp.printer] *)
  let rec cg_pp_ty ~root ~clique name out (ty: A.ty_expr) : unit =
    let recurse = cg_pp_ty ~root:false ~clique name in
    match ty with
    | A.Named_ty {name;_} ->
      if List.mem name clique then (
        (* use the recursion callback *)
        fpf out "!_pp_%s" (String.uncapitalize_ascii name)
      ) else (
        fpf out "%s.pp" (String.capitalize_ascii name)
      )
    | A.Uint -> fpf out "Bare.Pp.int64"
    | A.Int -> fpf out "Bare.Pp.int64"
    | A.U8 -> fpf out "Bare.Pp.int8"
    | A.I8 -> fpf out "Bare.Pp.int8"
    | A.U16 -> fpf out "Bare.Pp.int"
    | A.I16 -> fpf out "Bare.Pp.int"
    | A.U32 -> fpf out "Bare.Pp.int32"
    | A.I32 -> fpf out "Bare.Pp.int32"
    | A.U64 -> fpf out "Bare.Pp.int64"
    | A.I64 -> fpf out "Bare.Pp.int64"
    | A.F32 -> fpf out "Bare.Pp.float"
    | A.F64 -> fpf out "Bare.Pp.float"
    | A.Bool -> fpf out "Bare.Pp.bool"
    | A.String -> fpf out "Bare.Pp.string"
    | A.Data {len=_} -> fpf out "Bare.Pp.data"
    | A.Void -> fpf out "Bare.Pp.unit"
    | A.Optional ty ->
      fpf out "(@[<2>Bare.Pp.option@ %a@])" recurse ty
    | A.Array {ty; len=_} ->
      fpf out "(@[<2>Bare.Pp.array@ %a@])" recurse ty
    | A.Map (String, b) ->
      fpf out
        "(@[<v>fun out map ->@ Bare.Pp.iter@ \
         (@[fun out (xi,yi) ->@ \
         @[<2>Format.fprintf out \"(%%a -> %%a)\"@ %a xi@ %a yi@]@])@ out@ \
         (@[fun f ->@ Bare.String_map.iter (fun x y->f (x,y)) map@])@])"
        recurse String recurse b
    | A.Map (a, b) ->
      fpf out
        "(@[<v>Bare.Pp.list@ \
         (@[fun out (xi,yi) -> Format.fprintf out \"(%%a -> %%a)\" %a xi %a yi@])@])"
        recurse a recurse b
    | A.Struct l ->
      assert root; (* flattened *)
      fpf out "(@[<v2>@[<v>fun out x ->@ begin@]@ Format.fprintf out \"{ @@[\";@ ";
      List.iter
        (fun (name,ty) ->
          let field = spf "x.%s" name in
          fpf out "@[<2>Format.fprintf out \"%s=%%a;@@ \"@ \
                   %a@ %s@];@,"
            name recurse ty field)
        l;
      fpf out "Format.fprintf out \"@@]}\";@;<1 -2>end@]@])";
      ()

  let cg_pp_def_rhs_encode ~clique name out (def: A.ty_def_rhs) : unit =
    match def with
    | A.Atomic ty ->
      fpf out "%a out self" (cg_pp_ty ~clique ~root:true name) ty
    | A.Enum cases ->
      fpf out "@[<v>match self with@ ";
      List.iter
        (fun (s,_) ->
           let c = String.capitalize_ascii s in
           fpf out "| @[%s ->@ Format.fprintf out %S@]@," c c;
        ) cases;
      fpf out "@]"
    | A.Union l ->
      fpf out "@[<hv>match self with@ ";
      List.iteri
        (fun i ty ->
           let cstor = union_elt_name ~ty_name:name i ty in
           match ty with
           | A.Void | A.Named_ty {is_void=true;_} ->
             fpf out "| @[<v>%s ->@ Format.fprintf out %S@]@," cstor cstor
           | _ ->
             fpf out "| @[<v>%s x ->@ \
                      Format.fprintf out \"(@@[%s@@ %%a@@])\" %a x@]@,"
               cstor cstor (cg_pp_ty ~clique ~root:true name) ty)
        l

  (* define pretty printer for [def] *)
  let cg_pp ~clique name out def : unit =
    fpf out "@,@[<v2>let pp out (self:t) : unit =@ %a@]@,"
      (cg_pp_def_rhs_encode ~clique name) def

  let encode_ty_def_scc (self:t) ~pp (defs:A.ty_def list) : unit =
    match defs with
    | [{A.def=Atomic Void; name}] ->
      if !debug then Format.eprintf "skip void type %s@." name;
      ()
    | [d] ->
      let {A.name; def} = d in
      if !debug then Format.eprintf "codegen for type %s@." name;
      fpf self.out "@[<v2>module %s = struct@," (String.capitalize_ascii name);
      cg_ty_def_rhs_def ~lead:"type" ~self_ty:"t"
        name ~clique:[name] self.out def;
      fpf self.out "%a" (cg_ty_encode_decode ~clique:[name] name) def;
      if pp then fpf self.out "%a" (cg_pp ~clique:[name] name) def;
      fpf self.out "@]@.end@.@.";
      ()
    | [] -> assert false
    | defs ->
      (* first, declare all types in a mutually recursive block *)
      let clique = List.map (fun d->d.A.name) defs in
      if !debug then Format.eprintf "codegen for types [%s]@." (String.concat "," clique);
      fpf self.out "@[<v>";
      List.iteri
        (fun i {A.name; def} ->
          let lead = if i=0 then "type" else "and" in
          let self_ty = String.uncapitalize_ascii name in
          cg_ty_def_rhs_def ~lead ~self_ty name ~clique self.out def)
        defs;
      fpf self.out "@]@,";
      (* forward declarations for the mutually recursive functions *)
      List.iter
        (fun {A.name;_} ->
          let self_ty = String.uncapitalize_ascii name in
          fpf self.out "let _encode_%s = ref (fun _ _ -> assert false)@," self_ty;
          fpf self.out "let _decode_%s = ref (fun _ -> assert false)@," self_ty;
          if pp then fpf self.out "let _pp_%s = ref (fun _ _ -> assert false)@," self_ty;
        ) defs;
      (* now build one module for each type *)
      List.iter
        (fun {A.name; def} ->
          fpf self.out "@[<v2>module %s = struct@," (String.capitalize_ascii name);
          (* alias+redeclare type *)
          let self_ty = String.uncapitalize_ascii name in
          let self_ty' = spf "t = %s" self_ty in
          cg_ty_def_rhs_def ~lead:"type" ~self_ty:self_ty' name ~clique self.out def;
          fpf self.out "%a" (cg_ty_encode_decode ~clique name) def;
          if pp then fpf self.out "%a" (cg_pp ~clique name) def;
          (* fill forward references *)
          fpf self.out "@,(* fill forward declarations *)@,";
          fpf self.out "let () = _encode_%s := encode@," self_ty;
          fpf self.out "let () = _decode_%s := decode@," self_ty;
          if pp then (
            fpf self.out "let () = _pp_%s := pp@," self_ty;
          );
          fpf self.out "@]@.end@.@.")
        defs;
      ()

  let encode_ty_defs (self:t) ~pp (defs:A.ty_def list) : unit =
    let defs =
      A.flatten_types defs
      |> A.replace_void_defs
      |> A.Find_scc.top
    in
    List.iter (encode_ty_def_scc ~pp self) defs
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

let codegen ~standalone ~to_stdout ~out ~pp ~files defs : unit =
  let cg = CG.create() in
  CG.add_prelude ~filenames:files ~standalone cg;
  CG.encode_ty_defs cg ~pp defs;
  if !debug then Printf.eprintf "generate code into %S\n" out;
  if out <> "" then (
    let oc = open_out out in
    CG.write_code oc cg;
    flush oc;
    close_out oc;
  );
  if to_stdout then (
    CG.write_code stdout cg;
  );
  ()

let () =
  let files = ref [] in
  let cat = ref false in
  let out = ref "" in
  let pp = ref false in
  let standalone = ref false in
  let stdout = ref false in
  let opts = [
    "--cat", Arg.Set cat, " print type definitions";
    "-d", Arg.Set debug, " debug mode";
    "-o", Arg.Set_string out, " codegen: print code to given file";
    "--standalone", Arg.Set standalone, " generate standalone code";
    "--stdout", Arg.Set stdout, " codegen: print code to stdout";
    "--pp", Arg.Set pp, " codegen: generate pretty printer code";
  ] |> Arg.align in
  Arg.parse opts (fun f -> files := f :: !files) "usage: bare-codegen [opt]* file+";
  let files = List.rev !files in
  let tys = List.map parse_file files |> List.flatten in
  if !cat then (
    List.iter (fun td -> Format.printf "%a@.@." A.pp_ty_def td) tys;
  );
  if !stdout || !out <> "" then (
    codegen
      ~standalone:!standalone ~to_stdout:!stdout ~pp:!pp ~out:!out
      ~files
      tys
  );
  ()

