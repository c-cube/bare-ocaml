
module type S = sig
  type t
  val create : unit -> t
  val add_prelude : t -> unit

  val encode_ty_defs : t -> Ast.ty_def list -> unit

  val code : t -> string
  val write_code : out_channel -> t -> unit
end
