
module String_map : module type of Map.Make(String)

module Decode : sig
  type t = {
    bs: bytes;
    mutable off: int;
  }

  exception Error of string

  type 'a dec = t -> 'a

  val uint : t -> int64
  val int : t -> int64

  val u8 : t -> char
  val u16 : t -> int
  val u32 : t -> int32
  val u64 : t -> int64
  val i8 : t -> char
  val i16 : t -> int
  val i32 : t -> int32
  val i64 : t -> int64
  val bool : t -> bool

  val f32 : t -> float
  val f64 : t -> float

  val string : t -> string
  val data : t -> bytes
  val data_of : size:int -> t -> bytes

  val optional : 'a dec -> 'a option dec
end


module Encode : sig
  type t = Buffer.t

  type 'a enc = t -> 'a -> unit

  val uint : t -> int64 -> unit
  val int : t -> int64 -> unit

  val u8 : t -> char -> unit
  val u16 : t -> int -> unit
  val u32 : t -> int32 -> unit
  val u64 : t -> int64 -> unit
  val i8 : t -> char -> unit
  val i16 : t -> int -> unit
  val i32 : t -> int32 -> unit
  val i64 : t -> int64 -> unit
  val bool : t -> bool -> unit

  val f64 : t -> float -> unit

  val string : t -> string -> unit
  val data : t -> bytes -> unit
  val data_of : size:int -> t -> bytes -> unit

  val optional : 'a enc -> 'a option enc
end

val of_bytes_exn : ?off:int -> 'a Decode.dec -> bytes -> 'a
(** @raise Decode.Error if decoding fails *)

val of_bytes : ?off:int -> 'a Decode.dec -> bytes -> ('a, string) result

val of_string_exn : 'a Decode.dec -> string -> 'a
(** @raise Decode.Error if decoding fails *)

val of_string : 'a Decode.dec -> string -> ('a, string) result

val to_string : 'a Encode.enc -> 'a -> string

