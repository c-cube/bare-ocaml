
let spf = Printf.sprintf

module Decode = struct
  exception Error of string

  type t = {
    bs: bytes;
    mutable off: int;
  }

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
end

module Encode = struct
  type t = Buffer.t

  let uint (self:t) i : unit =
    let open Int64 in
    let rec loop i =
      if equal i (logand i 0b0111_1111L) then (
        let i = to_int i land 0xff in
        Buffer.add_char self (Char.chr i)
      ) else (
        (* set bit 8 to [1] *)
        let lsb = 0b1000_0000 lor to_int (logand 0b0111_1111L i) in
        let i = shift_right_logical i 7 in
        Buffer.add_char self (Char.chr lsb);
        loop i
      )
    in
    loop i

  let int (self:t) i =
    let open Int64 in
    let ui = logxor (shift_left i 1) (shift_right i 63) in
    uint self ui
end

(*$inject
  let to_s f x =
    let out = Buffer.create 32 in
    f out x;
    Buffer.contents out

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

(*$Q
  Q.(int64) (fun s -> \
    s = (of_s Decode.uint (to_s Encode.uint s)))
*)

(*$Q
  Q.(int64) (fun s -> \
    s = (of_s Decode.int (to_s Encode.int s)))
*)

(* TODO: some tests with qtest *)
