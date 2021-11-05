(* generated from "example2.bare" using bare-codegen *)
[@@@ocaml.warning "-26-27"]

(* embedded runtime library *)
module Bare = struct
  
module String_map = Map.Make(String)

let spf = Printf.sprintf

module Decode = struct
  exception Error of string

  type t = {
    bs: bytes;
    mutable off: int;
  }

  type 'a dec = t -> 'a

  let fail_ e = raise (Error e)
  let fail_eof_ what =
    fail_ (spf "unexpected end of input, expected %s" what)

  let uint (self:t) : int64 =
    let rec loop () =
      if self.off >= Bytes.length self.bs then fail_eof_ "uint";
      let c = Char.code (Bytes.get self.bs self.off) in
      self.off <- 1 + self.off; (* consume *)
      if c land 0b1000_0000 <> 0 then (
        let rest = loop() in
        let c = Int64.of_int (c land 0b0111_1111) in
        Int64.(logor (shift_left rest 7) c)
      ) else (
        Int64.of_int c (* done *)
      )
    in
    loop()

  let int (self:t) : int64 =
    let open Int64 in
    let i = uint self in
    let sign_bit = logand 0b1L i in (* true if negative *)
    let sign = equal sign_bit 0L in
    let res =
      if sign then (
        shift_right_logical i 1
      ) else (
        (* put sign back *)
        logor (shift_left 1L 63) (shift_right_logical (lognot i) 1)
      )
    in
    res

  let u8 self : char =
    let x = Bytes.get self.bs self.off in
    self.off <- self.off + 1;
    x
  let i8 = u8

  let u16 self =
    let x = Bytes.get_int16_le self.bs self.off in
    self.off <- self.off + 2;
    x
  let i16 = u16

  let u32 self =
    let x = Bytes.get_int32_le self.bs self.off in
    self.off <- self.off + 4;
    x
  let i32 = u32

  let u64 self =
    let i = Bytes.get_int64_le self.bs self.off in
    self.off <- 8 + self.off;
    i
  let i64 = u64

  let bool self : bool =
    let c = Bytes.get self.bs self.off in
    self.off <- 1 + self.off;
    Char.code c <> 0

  let f32 (self:t) : float =
    let i = i32 self in
    Int32.float_of_bits i

  let f64 (self:t) : float =
    let i = i64 self in
    Int64.float_of_bits i

  let data_of ~size self : bytes =
    let s = Bytes.sub self.bs self.off size in
    self.off <- self.off + size;
    s

  let data self : bytes =
    let size = uint self in
    if Int64.compare size (Int64.of_int Sys.max_string_length) > 0 then
      fail_ "string too large";
    let size = Int64.to_int size in (* fits, because of previous test *)
    data_of ~size self

  let string self : string =
    Bytes.unsafe_to_string (data self)

  let[@inline] optional dec self : _ option =
    let c = u8 self in
    if Char.code c = 0 then None else Some (dec self)
end

module Encode = struct
  type t = Buffer.t

  let of_buffer buf : t = buf

  type 'a enc = t -> 'a -> unit

  (* no need to check for overflow below *)
  external unsafe_chr : int -> char = "%identity"

  let uint (self:t) (i:int64) : unit =
    let module I = Int64 in
    let i = ref i in
    let continue = ref true in
    while !continue do
      let j = I.logand 0b0111_1111L !i in
      if !i = j then (
        continue := false;
        let j = I.to_int j in
        Buffer.add_char self (unsafe_chr j)
      ) else (
        (* set bit 8 to [1] *)
        let lsb = I.to_int (I.logor 0b1000_0000L j) in
        let lsb = (unsafe_chr lsb) in
        Buffer.add_char self lsb;
        i := I.shift_right_logical !i 7;
      )
    done

  let[@inline] int (self:t) i =
    let open Int64 in
    let ui = logxor (shift_left i 1) (shift_right i 63) in
    uint self ui

  let u8 self x = Buffer.add_char self x
  let i8 = u8
  let u16 self x = Buffer.add_int16_le self x
  let i16 = u16
  let u32 self x = Buffer.add_int32_le self x
  let i32 = u32
  let u64 self x = Buffer.add_int64_le self x
  let i64 = u64

  let bool self x = Buffer.add_char self (if x then Char.chr 1 else Char.chr 0)

  let f64 (self:t) x = Buffer.add_int64_le self (Int64.bits_of_float x)

  let data_of ~size self x =
    if size <> Bytes.length x then failwith "invalid length for Encode.data_of";
    Buffer.add_bytes self x

  let data self x =
    uint self (Int64.of_int (Bytes.length x));
    Buffer.add_bytes self x

  let string self x = data self (Bytes.unsafe_of_string x)

  let[@inline] optional enc self x : unit =
    match x with
    | None -> u8 self (Char.chr 0)
    | Some x ->
      u8 self (Char.chr 1);
      enc self x
end

module Pp = struct
  type 'a t = Format.formatter -> 'a -> unit
  type 'a iter = ('a -> unit) -> unit
  let unit out () = Format.pp_print_string out "()"
  let int8 out c = Format.fprintf out "%d" (Char.code c)
  let int out x = Format.fprintf out "%d" x
  let int32 out x = Format.fprintf out "%ld" x
  let int64 out x = Format.fprintf out "%Ld" x
  let float out x = Format.fprintf out "%h" x
  let bool = Format.pp_print_bool
  let string out x = Format.fprintf out "%S" x
  let data out x = string out (Bytes.unsafe_to_string x)
  let option ppelt out x = match x with
    | None -> Format.fprintf out "None"
    | Some x -> Format.fprintf out "(Some %a)" ppelt x
  let array ppelt out x =
    Format.fprintf out "[@[";
    Array.iteri (fun i x ->
        if i>0 then Format.fprintf out ";@ ";
        ppelt out x)
      x;
    Format.fprintf out "@]]"
  let iter ppelt out xs =
    Format.fprintf out "[@[";
    let i = ref 0 in
    xs (fun x ->
        if !i>0 then Format.fprintf out ",@ ";
        incr i;
        ppelt out x);
    Format.fprintf out "@]]"
  let list ppelt out l = iter ppelt out (fun f->List.iter f l)
end

let to_string (e:'a Encode.enc) (x:'a) =
  let buf = Buffer.create 32 in
  e buf x;
  Buffer.contents buf

let of_bytes_exn ?(off=0) dec bs =
  let i = {Decode.bs; off} in
  dec i

let of_bytes ?off dec bs =
  try Ok (of_bytes_exn ?off dec bs)
  with Decode.Error e -> Error e

let of_string_exn dec s = of_bytes_exn dec (Bytes.unsafe_of_string s)
let of_string dec s = of_bytes dec (Bytes.unsafe_of_string s)


(*$inject
  let to_s f x =
    let buf = Buffer.create 32 in
    let out = Encode.of_buffer buf in
    f out x;
    Buffer.contents buf

  let of_s f x =
    let i = {Decode.off=0; bs=Bytes.unsafe_of_string x} in
    f i
*)

(*$= & ~printer:Int64.to_string
  37L (of_s Decode.uint (to_s Encode.uint 37L))
  42L (of_s Decode.uint (to_s Encode.uint 42L))
  0L (of_s Decode.uint (to_s Encode.uint 0L))
  105542252L (of_s Decode.uint (to_s Encode.uint 105542252L))
  Int64.max_int (of_s Decode.uint (to_s Encode.uint Int64.max_int))
*)

(*$= & ~printer:Int64.to_string
  37L (of_s Decode.int (to_s Encode.int 37L))
  42L (of_s Decode.int (to_s Encode.int 42L))
  0L (of_s Decode.int (to_s Encode.int 0L))
  105542252L (of_s Decode.int (to_s Encode.int 105542252L))
  Int64.max_int (of_s Decode.int (to_s Encode.int Int64.max_int))
  Int64.min_int (of_s Decode.int (to_s Encode.int Int64.min_int))
  (-1209433446454112432L) (of_s Decode.int (to_s Encode.int (-1209433446454112432L)))
  (-3112855215860398414L) (of_s Decode.int (to_s Encode.int (-3112855215860398414L)))
*)

(*$=
  1 (let s = to_s Encode.int (-1209433446454112432L) in 0x1 land (Char.code s.[0]))
*)

(*$Q & ~count:1000
  Q.(int64) (fun s -> \
    s = (of_s Decode.uint (to_s Encode.uint s)))
  Q.(small_nat) (fun n -> \
    let n = Int64.of_int n in \
    n = (of_s Decode.uint (to_s Encode.uint n)))
*)

(*$Q & ~count:1000
  Q.(int64) (fun s -> \
    s = (of_s Decode.int (to_s Encode.int s)))
  Q.(small_signed_int) (fun n -> \
    let n = Int64.of_int n in \
    n = (of_s Decode.int (to_s Encode.int n)))
*)

(*$R
    for i=0 to 1_000 do
      let i = Int64.of_int i in
      assert_equal ~printer:Int64.to_string i (of_s Decode.int (to_s Encode.int i))
    done
*)

(*$R
    for i=0 to 1_000 do
      let i = Int64.of_int i in
      assert_equal ~printer:Int64.to_string i (of_s Decode.uint (to_s Encode.uint i))
    done
*)

(*$Q & ~count:1000
  Q.(string) (fun s -> \
    s = (of_s Decode.string (to_s Encode.string s)))
*)

end

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


