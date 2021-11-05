(* generated from "example2.bare" using bare-codegen *)
[@@@ocaml.warning "-26-27"]
module Bare = Bare_encoding
module Person = struct
  type t = {
    first: string;
    last: string;
  }
  
  (** @raise Bare.Decode.Error in case of error. *)
  let decode (dec: Bare.Decode.t) : t =
    let first = Bare.Decode.string dec in
    let last = Bare.Decode.string dec in
    {first; last; }
  
  let encode (enc: Bare.Encode.t) (self: t) : unit =
    begin
      Bare.Encode.string enc self.first;
      Bare.Encode.string enc self.last;
    end
  
end

type pTreeNode = {
  left: pTree;
  person: Person.t;
  right: pTree;
}
and pTree =
  | PTreeNil
  | PTreeNode of pTreeNode
  
let _encode_pTreeNode = ref (fun _ _ -> assert false)
let _decode_pTreeNode = ref (fun _ -> assert false)
let _encode_pTree = ref (fun _ _ -> assert false)
let _decode_pTree = ref (fun _ -> assert false)
module PTreeNode = struct
  type t = pTreeNode = {
    left: pTree;
    person: Person.t;
    right: pTree;
  }
  
  (** @raise Bare.Decode.Error in case of error. *)
  let decode (dec: Bare.Decode.t) : t =
    let left = !_decode_pTree dec in
    let person = Person.decode dec in
    let right = !_decode_pTree dec in
    {left; person; right; }
  
  let encode (enc: Bare.Encode.t) (self: t) : unit =
    begin
      !_encode_pTree enc self.left;
      Person.encode enc self.person;
      !_encode_pTree enc self.right;
    end
  
  (* fill forward declarations *)
  let () = _encode_pTreeNode := encode
  let () = _decode_pTreeNode := decode
  
end

module PTree = struct
  type t = pTree =
    | PTreeNil
    | PTreeNode of pTreeNode
    
  
  (** @raise Bare.Decode.Error in case of error. *)
  let decode (dec: Bare.Decode.t) : t =
    let tag = Bare.Decode.uint dec in
    match tag with
    | 0L -> PTreeNil
    | 1L -> PTreeNode (!_decode_pTreeNode dec)
    | _ -> raise (Bare.Decode.Error(Printf.sprintf "unknown union tag PTree.t: %Ld" tag))
    
  
  let encode (enc: Bare.Encode.t) (self: t) : unit =
    match self with
    | PTreeNil ->
      Bare.Encode.uint enc 0L
    | PTreeNode x ->
      Bare.Encode.uint enc 1L;
      !_encode_pTreeNode enc x
    
    
    (* fill forward declarations *)
    let () = _encode_pTree := encode
    let () = _decode_pTree := decode
    
end

type rec1 = {
  a1: string;
  r2: rec2 option;
}
and rec2 = {
  a2: int;
  r1: rec1 option;
}
let _encode_rec1 = ref (fun _ _ -> assert false)
let _decode_rec1 = ref (fun _ -> assert false)
let _encode_rec2 = ref (fun _ _ -> assert false)
let _decode_rec2 = ref (fun _ -> assert false)
module Rec1 = struct
  type t = rec1 = {
    a1: string;
    r2: rec2 option;
  }
  
  (** @raise Bare.Decode.Error in case of error. *)
  let decode (dec: Bare.Decode.t) : t =
    let a1 = Bare.Decode.string dec in
    let r2 = Bare.Decode.optional (fun dec -> !_decode_rec2 dec) dec in
    {a1; r2; }
  
  let encode (enc: Bare.Encode.t) (self: t) : unit =
    begin
      Bare.Encode.string enc self.a1;
      Bare.Encode.optional
        (fun enc xopt -> !_encode_rec2 enc xopt) enc self.r2;
    end
  
  (* fill forward declarations *)
  let () = _encode_rec1 := encode
  let () = _decode_rec1 := decode
  
end

module Rec2 = struct
  type t = rec2 = {
    a2: int;
    r1: rec1 option;
  }
  
  (** @raise Bare.Decode.Error in case of error. *)
  let decode (dec: Bare.Decode.t) : t =
    let a2 = Bare.Decode.i16 dec in
    let r1 = Bare.Decode.optional (fun dec -> !_decode_rec1 dec) dec in
    {a2; r1; }
  
  let encode (enc: Bare.Encode.t) (self: t) : unit =
    begin
      Bare.Encode.i16 enc self.a2;
      Bare.Encode.optional
        (fun enc xopt -> !_encode_rec1 enc xopt) enc self.r1;
    end
  
  (* fill forward declarations *)
  let () = _encode_rec2 := encode
  let () = _decode_rec2 := decode
  
end

type pTree2 =
  | PTree2_0
  | PTree2Node of pTree2Node
  
and pTree2Node = {
  left: pTree2;
  i: int64;
  right: pTree2;
}
let _encode_pTree2 = ref (fun _ _ -> assert false)
let _decode_pTree2 = ref (fun _ -> assert false)
let _encode_pTree2Node = ref (fun _ _ -> assert false)
let _decode_pTree2Node = ref (fun _ -> assert false)
module PTree2 = struct
  type t = pTree2 =
    | PTree2_0
    | PTree2Node of pTree2Node
    
  
  (** @raise Bare.Decode.Error in case of error. *)
  let decode (dec: Bare.Decode.t) : t =
    let tag = Bare.Decode.uint dec in
    match tag with
    | 0L -> PTree2_0
    | 1L -> PTree2Node (!_decode_pTree2Node dec)
    | _ -> raise (Bare.Decode.Error(Printf.sprintf "unknown union tag PTree2.t: %Ld" tag))
    
  
  let encode (enc: Bare.Encode.t) (self: t) : unit =
    match self with
    | PTree2_0 ->
      Bare.Encode.uint enc 0L
    | PTree2Node x ->
      Bare.Encode.uint enc 1L;
      !_encode_pTree2Node enc x
    
    
    (* fill forward declarations *)
    let () = _encode_pTree2 := encode
    let () = _decode_pTree2 := decode
    
end

module PTree2Node = struct
  type t = pTree2Node = {
    left: pTree2;
    i: int64;
    right: pTree2;
  }
  
  (** @raise Bare.Decode.Error in case of error. *)
  let decode (dec: Bare.Decode.t) : t =
    let left = !_decode_pTree2 dec in
    let i = Bare.Decode.int dec in
    let right = !_decode_pTree2 dec in
    {left; i; right; }
  
  let encode (enc: Bare.Encode.t) (self: t) : unit =
    begin
      !_encode_pTree2 enc self.left;
      Bare.Encode.int enc self.i;
      !_encode_pTree2 enc self.right;
    end
  
  (* fill forward declarations *)
  let () = _encode_pTree2Node := encode
  let () = _decode_pTree2Node := decode
  
end

module AllInts = struct
  type t = {
    i1: char;
    i2: char;
    i3: int;
    i4: int;
    i5: int32;
    i6: int32;
    i7: int64;
    i8_: int64;
    i9: int64;
    i10: int64;
  }
  
  (** @raise Bare.Decode.Error in case of error. *)
  let decode (dec: Bare.Decode.t) : t =
    let i1 = Bare.Decode.i8 dec in
    let i2 = Bare.Decode.u8 dec in
    let i3 = Bare.Decode.u16 dec in
    let i4 = Bare.Decode.i16 dec in
    let i5 = Bare.Decode.u32 dec in
    let i6 = Bare.Decode.i32 dec in
    let i7 = Bare.Decode.u64 dec in
    let i8_ = Bare.Decode.i64 dec in
    let i9 = Bare.Decode.uint dec in
    let i10 = Bare.Decode.int dec in
    {i1; i2; i3; i4; i5; i6; i7; i8_; i9; i10; }
  
  let encode (enc: Bare.Encode.t) (self: t) : unit =
    begin
      Bare.Encode.i8 enc self.i1;
      Bare.Encode.u8 enc self.i2;
      Bare.Encode.u16 enc self.i3;
      Bare.Encode.i16 enc self.i4;
      Bare.Encode.u32 enc self.i5;
      Bare.Encode.i32 enc self.i6;
      Bare.Encode.u64 enc self.i7;
      Bare.Encode.i64 enc self.i8_;
      Bare.Encode.uint enc self.i9;
      Bare.Encode.int enc self.i10;
    end
  
end


