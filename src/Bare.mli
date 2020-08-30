
module Decode : sig
  type t = {
    bs: bytes;
    mutable off: int;
  }

  exception Error of string

  val uint : t -> int64
  val int : t -> int64
end


module Encode : sig
  type t = Buffer.t

  val uint : t -> int64 -> unit
  val int : t -> int64 -> unit

end


