(** BARE runtime library.

    See {{: https://baremessages.org/} the spec}. *)

module String_map : module type of Map.Make (String)

type bslice = {
  bs: bytes;
  mutable off: int;
  mutable len: int;
}

type 'a input = {
  read_byte: 'a -> char;
      (** Read a single byte.
      @raise Invalid_argument if input is exhausted *)
  read_i16: 'a -> int;
      (** Read 2 bytes, in little endian.
      @raise Invalid_argument if input is exhausted *)
  read_i32: 'a -> int32;
      (** Read 4 bytes, in little endian.
      @raise Invalid_argument if input is exhausted *)
  read_i64: 'a -> int64;
      (** Read 8 bytes, in little endian.
      @raise Invalid_argument if input is exhausted *)
  read_exact: 'a -> bytes -> int -> int -> unit;
      (** [read_exact buf i len] reads [len] bytes into [buf], starting at offset [i]
      @raise Invalid_argument if input has less than [len] bytes *)
}
(** Input type.

    An input is a source of bytes, used to decode. *)

(** Decoders.

    This module provides a decoder type {!Decode.t} to hold the decoding state,
    along with functions to decode specific primitives. *)
module Decode : sig
  type t

  val of_input : 'a input -> 'a -> t
  (** [create input] makes a decoder for the given input.
      @since 0.2 *)

  val of_string : ?off:int -> ?len:int -> string -> t
  (** Decoder reading from the string.
      @param off initial offset in the string
      @param len length of the slice of the string
      @raises Invalid_arg if [off,off+len] is not a valid slice in the string.
      @since 0.2
  *)

  val of_bytes : ?off:int -> ?len:int -> bytes -> t
  (** See {!of_string}
      @since 0.2
  *)

  val of_bslice : bslice -> t
  (** Decode from a slice (mutates it)
      @since NEXT_RELEASE *)

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

type 'a output = {
  write_byte: 'a -> char -> unit;  (** Write a single byte. *)
  write_i16: 'a -> int -> unit;  (** Write 2 bytes, in little endian. *)
  write_i32: 'a -> int32 -> unit;  (** Write 4 bytes, in little endian. *)
  write_i64: 'a -> int64 -> unit;  (** Write 8 bytes, in little endian. *)
  write_exact: 'a -> bytes -> int -> int -> unit;
      (** [write_exact b i len] writes the slice of length [len]
      of [b] starting at [i]. *)
  flush: 'a -> unit;
      (** Non specified hint that the data may be flushed onto the disk
      or network. Doing nothing is acceptable when this makes no sense
      (e.g. when writing to a {!Buffer.t}). *)
}
(** Output

    An output is a sink where one can write bytes to encode data to BARE. *)

(** Encoder type and some encoding utils.

    The type {!Encode.t} is passed to type-specific encoders
    so they can serialize values into bytes.
    This module also provides a host of functions that encode specific
    types (mostly integers and strings) into the {!Encode.t}. *)
module Encode : sig
  type t
  (** Encoding state. *)

  val of_output : 'i output -> 'i -> t
  (** Encoding state that writes into the given output.
      @since 0.2 *)

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

val of_bytes_exn : ?off:int -> ?len:int -> 'a Decode.dec -> bytes -> 'a
(** [of_bytes_exn dec bs] uses [dec] to decode a value of type ['a]
    from bytes stored in [bs].
    @param off the initial offset in [bs] (default 0)
    @raise Decode.Error if decoding fails *)

val of_bytes :
  ?off:int -> ?len:int -> 'a Decode.dec -> bytes -> ('a, string) result
(** Same as {!of_bytes_exn} but doesn't raise. *)

val of_string_exn : ?off:int -> ?len:int -> 'a Decode.dec -> string -> 'a
(** Decode a value stored in the string.
    See {of_bytes_exn} for more details.
    @raise Decode.Error if decoding fails *)

val of_string :
  ?off:int -> ?len:int -> 'a Decode.dec -> string -> ('a, string) result
(** Safe version of {!of_string_exn} *)

val to_string : 'a Encode.enc -> 'a -> string
(** Encode a value of type ['a] into a string using the given encoder. *)
