
(** BARE runtime library.

    See {{: https://baremessages.org/} the spec}. *)

module String_map : module type of Map.Make(String)

(** Decoders.

    This module provides a decoder type {!Decode.t} to hold the decoding state,
    along with functions to decode specific primitives. *)
module Decode : sig
  type t = {
    bs: bytes;
    mutable off: int;
  }
  (** A decoder state, operating on a slice of bytes. *)

  exception Error of string

  type 'a dec = t -> 'a
  (** A decoder for values of type ['a].
      Decoders will raise {!Error} to indicate failure. *)

  val uint : t -> int64
  (** Decode a varint-encoded unsigned integer *)

  val int : t -> int64
  (** Decode a varint-encoded integer *)

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


(** Encoder type and some encoding utils.

    The type {!Encode.t} is passed to type-specific encoders
    so they can serialize values into bytes.
    This module also provides a host of functions that encode specific
    types (mostly integers and strings) into the {!Encode.t}. *)
module Encode : sig
  type t
  (** Encoding state. *)

  val of_buffer : Buffer.t -> t
  (** Encoding state that emits bytes into the given buffer.
      After using a type encoder on this encoder, the buffer will contain
      the bytes. *)

  type 'a enc = t -> 'a -> unit
  (** A type encoder for values of type ['a]. *)

  val uint : t -> int64 -> unit
  (** Encode unsigned integers using the varint encoding *)

  val int : t -> int64 -> unit
  (** Encode signed integers using the varint encoding *)

  val u8 : t -> char -> unit
  (** Encode a single byte *)

  val u16 : t -> int -> unit
  (** Encode a two byte unsigned integers in little endian *)

  val u32 : t -> int32 -> unit
  (** Encode an unsigned int32 integer in little endian *)

  val u64 : t -> int64 -> unit
  (** Encode an unsigned int64 integer in little endian *)

  val i8 : t -> char -> unit
  (** Same as {!u8} *)

  val i16 : t -> int -> unit
  (** Encode a two byte unsigned integers in little endian *)

  val i32 : t -> int32 -> unit
  (** Encode a int32 integer in little endian *)

  val i64 : t -> int64 -> unit
  (** Encode a int64 integer in little endian *)

  val bool : t -> bool -> unit
  (** Encode a boolean as a single byte [0] or [1]. *)

  val f64 : t -> float -> unit
  (** Encode a float. *)

  val string : t -> string -> unit
  (** Encode a string, prefixed by its length *)

  val data : t -> bytes -> unit
  (** Encode a blob, prefixed by its length *)

  val data_of : size:int -> t -> bytes -> unit
  (** Encode a blob of fixed size.
      @raise Failure if the blob's length in bytes is not equal to [size] *)

  val optional : 'a enc -> 'a option enc
  (** Encode an optional value, as either [0] for [None]
      or [1] followed by the encoding of [x] for [Some x]. *)
end

(** Pretty printer utils, using {!module:Format}. *)
module Pp : sig
  type 'a t = Format.formatter -> 'a -> unit
  (** A pretty printer for values of type ['a] *)

  type 'a iter = ('a -> unit) -> unit
  val unit : unit t
  val int : int t
  val int8 : char t
  val int32 : int32 t
  val int64 : int64 t
  val float : float t
  val bool : bool t
  val string : string t
  val data : bytes t
  val option : 'a t -> 'a option t
  val array : 'a t -> 'a array t
  val iter : 'a t -> 'a iter t
  val list : 'a t -> 'a list t
end

val of_bytes_exn : ?off:int -> 'a Decode.dec -> bytes -> 'a
(** @raise Decode.Error if decoding fails *)

val of_bytes : ?off:int -> 'a Decode.dec -> bytes -> ('a, string) result

val of_string_exn : 'a Decode.dec -> string -> 'a
(** @raise Decode.Error if decoding fails *)

val of_string : 'a Decode.dec -> string -> ('a, string) result

val to_string : 'a Encode.enc -> 'a -> string

