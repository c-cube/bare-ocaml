module String_map = Map.Make (String)

type 'a input = {
  read_byte: 'a -> char;
  read_i16: 'a -> int;
  read_i32: 'a -> int32;
  read_i64: 'a -> int64;
  read_exact: 'a -> bytes -> int -> int -> unit;
}

type bslice = {
  bs: bytes;
  mutable off: int;
  mutable len: int;
}

module Input_of_bslice_ = struct
  type t = bslice

  let[@inline] consume_ (self : t) n : int =
    if self.len < n then invalid_arg "input exhausted";
    let off = self.off in
    self.off <- self.off + n;
    self.len <- self.len - n;
    off

  let[@inline] read_byte self =
    let off = consume_ self 1 in
    let c = Bytes.get self.bs off in
    c

  let[@inline] read_i16 (self : t) =
    let off = consume_ self 2 in
    let r = Bytes.get_int16_le self.bs off in
    r

  let[@inline] read_i32 (self : t) =
    let off = consume_ self 4 in
    let r = Bytes.get_int32_le self.bs off in
    r

  let read_i64 (self : t) =
    let off = consume_ self 8 in
    let r = Bytes.get_int64_le self.bs off in
    r

  let read_exact self into i len =
    let off = consume_ self len in
    Bytes.blit self.bs off into i len
end

let input_of_bslice : bslice input =
  let open Input_of_bslice_ in
  { read_byte; read_i64; read_i16; read_i32; read_exact }

let bslice_of_bytes ?(off = 0) ?len (b : bytes) : bslice =
  let len =
    match len with
    | None -> Bytes.length b - off
    | Some l -> l
  in
  if off + len > Bytes.length b then invalid_arg "input_of_bytes";
  { bs = b; off; len }

module Decode = struct
  type t = D : 'a input * 'a -> t

  let[@inline] of_input (i : _ input) x : t = D (i, x)
  let of_bslice b : t = of_input input_of_bslice b
  let of_bytes ?off ?len b : t = of_bslice @@ bslice_of_bytes ?off ?len b
  let of_string ?off ?len s = of_bytes ?off ?len (Bytes.unsafe_of_string s)

  type 'a dec = t -> 'a

  let uint (D (i, x)) : int64 =
    let[@unroll 2] rec loop () =
      let c = i.read_byte x in
      let c = Char.code c in
      if c land 0b1000_0000 <> 0 then (
        let rest = loop () in
        let c = Int64.of_int (c land 0b0111_1111) in
        Int64.(logor (shift_left rest 7) c)
      ) else
        Int64.of_int c (* done *)
    in
    loop ()

  let int (self : t) : int64 =
    let open Int64 in
    let i = uint self in
    let sign_bit = logand 0b1L i in
    (* true if negative *)
    let sign = equal sign_bit 0L in
    let res =
      if sign then
        shift_right_logical i 1
      else
        (* put sign back *)
        logor (shift_left 1L 63) (shift_right_logical (lognot i) 1)
    in
    res

  let[@inline] i8 (D (i, x)) : char = i.read_byte x
  let u8 = i8
  let[@inline] i16 (D (i, x)) = i.read_i16 x
  let u16 = i16
  let[@inline] i32 (D (i, x)) = i.read_i32 x
  let u32 = i32
  let[@inline] i64 (D (i, x)) = i.read_i64 x
  let u64 = i64

  let[@inline] bool self : bool =
    let c = i8 self in
    Char.code c <> 0

  let f32 (self : t) : float =
    let i = i32 self in
    Int32.float_of_bits i

  let f64 (self : t) : float =
    let i = i64 self in
    Int64.float_of_bits i

  let data_of ~size (D (i, x)) : bytes =
    let b = Bytes.create size in
    i.read_exact x b 0 size;
    b

  let data self : bytes =
    let size = uint self in
    if Int64.compare size (Int64.of_int Sys.max_string_length) > 0 then
      invalid_arg "Decode.data: string too large";
    let size = Int64.to_int size in
    (* fits, because of previous test *)
    data_of ~size self

  let string self : string = Bytes.unsafe_to_string (data self)

  let[@inline] optional dec self : _ option =
    let c = u8 self in
    if Char.code c = 0 then
      None
    else
      Some (dec self)
end

type 'a output = {
  write_byte: 'a -> char -> unit;
  write_i16: 'a -> int -> unit;
  write_i32: 'a -> int32 -> unit;
  write_i64: 'a -> int64 -> unit;
  write_exact: 'a -> bytes -> int -> int -> unit;
  flush: 'a -> unit;
}

let output_of_buffer : Buffer.t output =
  {
    write_byte = Buffer.add_char;
    write_i16 = Buffer.add_int16_le;
    write_i32 = Buffer.add_int32_le;
    write_i64 = Buffer.add_int64_le;
    write_exact = Buffer.add_subbytes;
    flush = ignore;
  }

module Encode = struct
  type t = E : 'a output * 'a -> t

  let[@inline] of_output (o : _ output) x : t = E (o, x)
  let[@inline] of_buffer buf : t = of_output output_of_buffer buf

  type 'a enc = t -> 'a -> unit

  (* no need to check for overflow below *)
  external unsafe_chr : int -> char = "%identity"

  let uint (self : t) (i : int64) : unit =
    let module I = Int64 in
    let (E (o, st)) = self in
    let i = ref i in
    let continue = ref true in
    while !continue do
      let j = I.logand 0b0111_1111L !i in
      if !i = j then (
        continue := false;
        let j = I.to_int j in
        o.write_byte st (unsafe_chr j)
      ) else (
        (* set bit 8 to [1] *)
        let lsb = I.to_int (I.logor 0b1000_0000L j) in
        let lsb = unsafe_chr lsb in
        o.write_byte st lsb;
        i := I.shift_right_logical !i 7
      )
    done

  let[@inline] int (self : t) i =
    let open Int64 in
    let ui = logxor (shift_left i 1) (shift_right i 63) in
    uint self ui

  let[@inline] i8 (self : t) x =
    let (E (o, st)) = self in
    o.write_byte st x

  let u8 = i8

  let[@inline] i16 (self : t) x =
    let (E (o, st)) = self in
    o.write_i16 st x

  let u16 = i16

  let[@inline] i32 (self : t) x =
    let (E (o, st)) = self in
    o.write_i32 st x

  let u32 = i32

  let[@inline] i64 (self : t) x =
    let (E (o, st)) = self in
    o.write_i64 st x

  let u64 = i64

  let bool self x =
    i8 self
      (if x then
         Char.chr 1
       else
         Char.chr 0)

  let f64 (self : t) x = i64 self (Int64.bits_of_float x)

  let data_of ~size (self : t) x =
    if size <> Bytes.length x then failwith "invalid length for Encode.data_of";
    let (E (o, st)) = self in
    o.write_exact st x 0 size

  let data (self : t) x =
    uint self (Int64.of_int (Bytes.length x));
    let (E (o, st)) = self in
    o.write_exact st x 0 (Bytes.length x)

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

  let option ppelt out x =
    match x with
    | None -> Format.fprintf out "None"
    | Some x -> Format.fprintf out "(Some %a)" ppelt x

  let array ppelt out x =
    Format.fprintf out "[@[";
    Array.iteri
      (fun i x ->
        if i > 0 then Format.fprintf out ";@ ";
        ppelt out x)
      x;
    Format.fprintf out "@]]"

  let iter ppelt out xs =
    Format.fprintf out "[@[";
    let i = ref 0 in
    xs (fun x ->
        if !i > 0 then Format.fprintf out ",@ ";
        incr i;
        ppelt out x);
    Format.fprintf out "@]]"

  let list ppelt out l = iter ppelt out (fun f -> List.iter f l)
end

let to_string (e : 'a Encode.enc) (x : 'a) =
  let buf = Buffer.create 32 in
  e (Encode.of_buffer buf) x;
  Buffer.contents buf

let of_bytes_exn ?off ?len dec b =
  let i = Decode.of_bytes ?off ?len b in
  dec i

let of_bytes ?off ?len dec bs =
  try Ok (of_bytes_exn ?off ?len dec bs) with
  | Invalid_argument e | Failure e -> Error e
  | End_of_file -> Error "end of file"

let of_string_exn ?off ?len dec s =
  of_bytes_exn ?off ?len dec (Bytes.unsafe_of_string s)

let of_string ?off ?len dec s =
  of_bytes ?off ?len dec (Bytes.unsafe_of_string s)
