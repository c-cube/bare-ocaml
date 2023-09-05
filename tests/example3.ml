(* generated from "example3.bare" using bare-codegen *)
[@@@ocaml.warning "-26-27"]
module Bare = Bare_encoding
type arg_value =
  | Int64 of int64
  | String of string
  | Bool of bool
  | Float64 of float
  | Void
  | Arg_value_5 of arg array
  
and arg = {
  key: string;
  value: arg_value;
}
let _encode_arg_value = ref (fun _ _ -> assert false)
let _decode_arg_value = ref (fun _ -> assert false)
let _pp_arg_value = ref (fun _ _ -> assert false)
let _encode_arg = ref (fun _ _ -> assert false)
let _decode_arg = ref (fun _ -> assert false)
let _pp_arg = ref (fun _ _ -> assert false)
module Arg_value = struct
  type t = arg_value =
    | Int64 of int64
    | String of string
    | Bool of bool
    | Float64 of float
    | Void
    | Arg_value_5 of arg array
    
  
  (** @raise Invalid_argument in case of error. *)
  let decode (dec: Bare.Decode.t) : t =
    let tag = Bare.Decode.uint dec in
    match tag with
    | 0L -> Int64 (Bare.Decode.i64 dec)
    | 1L -> String (Bare.Decode.string dec)
    | 2L -> Bool (Bare.Decode.bool dec)
    | 3L -> Float64 (Bare.Decode.f64 dec)
    | 4L -> Void
    | 5L ->
      Arg_value_5 ((let len = Bare.Decode.uint dec in
                    if len>Int64.of_int Sys.max_array_length then invalid_arg "array too big";
                    Array.init (Int64.to_int len) (fun _ -> !_decode_arg dec)))
    | _ -> invalid_arg
      (Printf.sprintf "unknown union tag Arg_value.t: %Ld" tag)
    
  
  let encode (enc: Bare.Encode.t) (self: t) : unit =
    match self with
    | Int64 x ->
      Bare.Encode.uint enc 0L;
      Bare.Encode.i64 enc x
    | String x ->
      Bare.Encode.uint enc 1L;
      Bare.Encode.string enc x
    | Bool x ->
      Bare.Encode.uint enc 2L;
      Bare.Encode.bool enc x
    | Float64 x ->
      Bare.Encode.uint enc 3L;
      Bare.Encode.f64 enc x
    | Void ->
      Bare.Encode.uint enc 4L
    | Arg_value_5 x ->
      Bare.Encode.uint enc 5L;
      (let arr = x in
       Bare.Encode.uint enc (Int64.of_int (Array.length arr));
       Array.iter (fun xi -> !_encode_arg enc xi) arr)
    
    
    let pp out (self:t) : unit =
      match self with
      | Int64 x ->
        Format.fprintf out "(@[Int64@ %a@])" Bare.Pp.int64 x
      | String x ->
        Format.fprintf out "(@[String@ %a@])" Bare.Pp.string x
      | Bool x ->
        Format.fprintf out "(@[Bool@ %a@])" Bare.Pp.bool x
      | Float64 x ->
        Format.fprintf out "(@[Float64@ %a@])" Bare.Pp.float x
      | Void ->
        Format.fprintf out "Void"
      | Arg_value_5 x ->
        Format.fprintf out "(@[Arg_value_5@ %a@])" (Bare.Pp.array !_pp_arg) x
      
      
      (* fill forward declarations *)
      let () = _encode_arg_value := encode
      let () = _decode_arg_value := decode
      let () = _pp_arg_value := pp
      
end

module Arg = struct
  type t = arg = {
    key: string;
    value: arg_value;
  }
  
  (** @raise Invalid_argument in case of error. *)
  let decode (dec: Bare.Decode.t) : t =
    let key = Bare.Decode.string dec in
    let value = !_decode_arg_value dec in
    {key; value; }
  
  let encode (enc: Bare.Encode.t) (self: t) : unit =
    begin
      Bare.Encode.string enc self.key;
      !_encode_arg_value enc self.value;
    end
  
  let pp out (self:t) : unit =
    (fun out x ->
     begin
       Format.fprintf out "{ @[";
       Format.fprintf out "key=%a;@ " Bare.Pp.string x.key;
       Format.fprintf out "value=%a;@ " !_pp_arg_value x.value;
       Format.fprintf out "@]}";
     end) out self
  
  (* fill forward declarations *)
  let () = _encode_arg := encode
  let () = _decode_arg := decode
  let () = _pp_arg := pp
  
end


