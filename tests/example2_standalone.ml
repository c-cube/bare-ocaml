(* generated from "example2.bare" using bare-codegen *)
[@@@ocaml.warning "-26-27"]

(* embedded runtime library *)
module Bare = struct
  
module String_map = Map.Make(String)

module type INPUT = sig
  val read_byte : unit -> char
  val read_i16 : unit -> int
  val read_i32 : unit -> int32
  val read_i64 : unit -> int64
  val read_exact : bytes -> int -> int -> unit
end
type input = (module INPUT)

let input_of_bytes ?(off=0) ?len (b:bytes) : input =
  let off = ref off in
  let len = match len with
    | None -> Bytes.length b - !off
    | Some l -> l
  in
  if !off + len > Bytes.length b then invalid_arg "input_of_bytes";
  let[@inline] check_ n = if !off + n > len then invalid_arg "input exhausted" in
  let module M = struct
    let read_byte () = check_ 1; let c = Bytes.get b !off in incr off; c
    let read_i16 () = check_ 2; let r = Bytes.get_int16_le b !off in off := !off + 2; r
    let read_i32 () = check_ 4; let r = Bytes.get_int32_le b !off in off := !off + 4; r
    let read_i64 () = check_ 8; let r = Bytes.get_int64_le b !off in off := !off + 8; r
    let read_exact into i len = check_ len; Bytes.blit b !off into i len; off := !off + len
  end in
  (module M)

module Decode = struct
  type t = input

  let[@inline] of_input (i:input) : t = i
  let of_bytes ?off ?len b = of_input (input_of_bytes ?off ?len b)
  let of_string ?off ?len s = of_bytes ?off ?len (Bytes.unsafe_of_string s)

  type 'a dec = t -> 'a

  let uint (self:t) : int64 =
    let rec loop () =
      let c = let (module M) = self in M.read_byte() in
      let c = Char.code c in
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

  let i8 (self:t) : char = let (module M) = self in M.read_byte()
  let u8 = i8

  let i16 (self:t) = let (module M) = self in M.read_i16()
  let u16 = i16

  let i32 (self:t) = let (module M) = self in M.read_i32()
  let u32 = i32

  let i64 (self:t) = let (module M) = self in M.read_i64()
  let u64 = i64

  let[@inline] bool self : bool =
    let c = i8 self in
    Char.code c <> 0

  let f32 (self:t) : float =
    let i = i32 self in
    Int32.float_of_bits i

  let f64 (self:t) : float =
    let i = i64 self in
    Int64.float_of_bits i

  let data_of ~size (self:t) : bytes =
    let b = Bytes.create size in
    let (module M) = self in
    M.read_exact b 0 size;
    b

  let data self : bytes =
    let size = uint self in
    if Int64.compare size (Int64.of_int Sys.max_string_length) > 0 then
      invalid_arg "Decode.data: string too large";
    let size = Int64.to_int size in (* fits, because of previous test *)
    data_of ~size self

  let string self : string =
    Bytes.unsafe_to_string (data self)

  let[@inline] optional dec self : _ option =
    let c = u8 self in
    if Char.code c = 0 then None else Some (dec self)
end

module type OUTPUT = sig
  val write_byte : char -> unit
  val write_i16 : int -> unit
  val write_i32 : int32 -> unit
  val write_i64 : int64 -> unit
  val write_exact : bytes -> int -> int -> unit
  val flush : unit -> unit
end

type output = (module OUTPUT)

let output_of_buffer (buf:Buffer.t) : output =
  let module M = struct
    let[@inline] write_byte c = Buffer.add_char buf c
    let[@inline] write_i16 c = Buffer.add_int16_le buf c
    let[@inline] write_i32 c = Buffer.add_int32_le buf c
    let[@inline] write_i64 c = Buffer.add_int64_le buf c
    let write_exact b i len = Buffer.add_subbytes buf b i len
    let flush _ = ()
  end in
  (module M)

module Encode = struct
  type t = output

  let[@inline] of_output (o:output) : t = o
  let[@inline] of_buffer buf : t = of_output @@ output_of_buffer buf

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
        let (module M) = self in
        M.write_byte (unsafe_chr j)
      ) else (
        (* set bit 8 to [1] *)
        let lsb = I.to_int (I.logor 0b1000_0000L j) in
        let lsb = (unsafe_chr lsb) in
        let (module M) = self in
        M.write_byte lsb;
        i := I.shift_right_logical !i 7;
      )
    done

  let[@inline] int (self:t) i =
    let open Int64 in
    let ui = logxor (shift_left i 1) (shift_right i 63) in
    uint self ui

  let[@inline] i8 (self:t) x = let (module M) = self in M.write_byte x
  let u8 = i8
  let[@inline] i16 (self:t) x = let (module M) = self in M.write_i16 x
  let u16 = i16
  let[@inline] i32 (self:t) x = let (module M) = self in M.write_i32 x
  let u32 = i32
  let[@inline] i64 (self:t) x = let (module M) = self in M.write_i64 x
  let u64 = i64

  let bool self x = i8 self (if x then Char.chr 1 else Char.chr 0)

  let f64 (self:t) x = i64 self (Int64.bits_of_float x)

  let data_of ~size (self:t) x =
    if size <> Bytes.length x then failwith "invalid length for Encode.data_of";
    let (module M) = self in
    M.write_exact x 0 size

  let data (self:t) x =
    uint self (Int64.of_int (Bytes.length x));
    let (module M) = self in
    M.write_exact x 0 (Bytes.length x)

  let[@inline] string self x = data self (Bytes.unsafe_of_string x)

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
  e (Encode.of_buffer buf) x;
  Buffer.contents buf

let of_bytes_exn ?off ?len dec b =
  let i = Decode.of_bytes ?off ?len b in
  dec i

let of_bytes ?off ?len dec bs =
  try Ok (of_bytes_exn ?off ?len dec bs)
  with
  | Invalid_argument e | Failure e -> Error e
  | End_of_file -> Error "end of file"

let of_string_exn ?off ?len dec s = of_bytes_exn ?off ?len dec (Bytes.unsafe_of_string s)
let of_string ?off ?len dec s = of_bytes ?off ?len dec (Bytes.unsafe_of_string s)


(*$inject
  let to_s f x =
    let buf = Buffer.create 32 in
    let out = Encode.of_buffer buf in
    f out x;
    Buffer.contents buf

  let of_s f x =
    let i = Decode.of_string x in
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
  
  (** @raise Invalid_argument in case of error. *)
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
  
  (** @raise Invalid_argument in case of error. *)
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
    
  
  (** @raise Invalid_argument in case of error. *)
  let decode (dec: Bare.Decode.t) : t =
    let tag = Bare.Decode.uint dec in
    match tag with
    | 0L -> PTreeNil
    | 1L -> PTreeNode (!_decode_pTreeNode dec)
    | _ -> invalid_arg (Printf.sprintf "unknown union tag PTree.t: %Ld" tag)
    
  
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
  
  (** @raise Invalid_argument in case of error. *)
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
  
  (** @raise Invalid_argument in case of error. *)
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
  | Void
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
    | Void
    | PTree2Node of pTree2Node
    
  
  (** @raise Invalid_argument in case of error. *)
  let decode (dec: Bare.Decode.t) : t =
    let tag = Bare.Decode.uint dec in
    match tag with
    | 0L -> Void
    | 1L -> PTree2Node (!_decode_pTree2Node dec)
    | _ -> invalid_arg (Printf.sprintf "unknown union tag PTree2.t: %Ld" tag)
    
  
  let encode (enc: Bare.Encode.t) (self: t) : unit =
    match self with
    | Void ->
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
  
  (** @raise Invalid_argument in case of error. *)
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
  
  (** @raise Invalid_argument in case of error. *)
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


