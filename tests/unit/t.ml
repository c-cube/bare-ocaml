open Bare_encoding
module Q = QCheck

let spf = Printf.sprintf

let to_s f x =
  let buf = Buffer.create 32 in
  let out = Encode.of_buffer buf in
  f out x;
  Buffer.contents buf

let of_s f x =
  let i = Decode.of_string x in
  f i

let q_suite : QCheck.Test.t list ref = ref []

let add_q_test ?count ~name gen prop : unit =
  q_suite := QCheck.Test.make ?count ~name gen prop :: !q_suite

let t_eq ~printer a b =
  if a <> b then
    failwith (spf "should be equal: %s and %s" (printer a) (printer b))

let t_eq_i64 = t_eq ~printer:Int64.to_string
let t_eq_int = t_eq ~printer:string_of_int
let () = t_eq_i64 37L (of_s Decode.uint (to_s Encode.uint 37L))
let () = t_eq_i64 42L (of_s Decode.uint (to_s Encode.uint 42L))
let () = t_eq_i64 0L (of_s Decode.uint (to_s Encode.uint 0L))
let () = t_eq_i64 105542252L (of_s Decode.uint (to_s Encode.uint 105542252L))

let () =
  t_eq_i64 Int64.max_int (of_s Decode.uint (to_s Encode.uint Int64.max_int))

let () = t_eq_i64 37L (of_s Decode.int (to_s Encode.int 37L))
let () = t_eq_i64 42L (of_s Decode.int (to_s Encode.int 42L))
let () = t_eq_i64 0L (of_s Decode.int (to_s Encode.int 0L))
let () = t_eq_i64 105542252L (of_s Decode.int (to_s Encode.int 105542252L))

let () =
  t_eq_i64 Int64.max_int (of_s Decode.int (to_s Encode.int Int64.max_int))

let () =
  t_eq_i64 Int64.min_int (of_s Decode.int (to_s Encode.int Int64.min_int))

let () =
  t_eq_i64 (-1209433446454112432L)
    (of_s Decode.int (to_s Encode.int (-1209433446454112432L)))

let () =
  t_eq_i64 (-3112855215860398414L)
    (of_s Decode.int (to_s Encode.int (-3112855215860398414L)))

let () =
  t_eq_int 1
    (let s = to_s Encode.int (-1209433446454112432L) in
     0x1 land Char.code s.[0])

let () =
  add_q_test ~count:1000 ~name:__LOC__
    Q.(int64)
    (fun s -> s = of_s Decode.uint (to_s Encode.uint s))

let () =
  add_q_test ~count:1000 ~name:__LOC__
    Q.(small_nat)
    (fun n ->
      let n = Int64.of_int n in
      n = of_s Decode.uint (to_s Encode.uint n))

let () =
  add_q_test ~count:1000 ~name:__LOC__
    Q.(int64)
    (fun s -> s = of_s Decode.int (to_s Encode.int s))

let () =
  add_q_test ~count:1000 ~name:__LOC__
    Q.(small_signed_int)
    (fun n ->
      let n = Int64.of_int n in
      n = of_s Decode.int (to_s Encode.int n))

let () =
  for i = 0 to 1_000 do
    let i = Int64.of_int i in
    t_eq_i64 i (of_s Decode.int (to_s Encode.int i))
  done

let () =
  for i = 0 to 1_000 do
    let i = Int64.of_int i in
    t_eq_i64 i (of_s Decode.uint (to_s Encode.uint i))
  done

let () =
  add_q_test ~count:1000 ~name:__LOC__
    Q.(string)
    (fun s -> s = of_s Decode.string (to_s Encode.string s))

let () = QCheck_base_runner.run_tests_main !q_suite
