module E = Example3
module Bare = Bare_encoding

let () =
  print_endline ">>> run encoding test 3";
  let a = E.Arg_value_5 [| { E.key = "k"; value = E.Bool true } |] in
  let s = Bare.to_string E.Arg_value.encode a in
  Printf.printf "p1:\n%s\n" @@ Hex.hexdump_s (Hex.of_string s);
  let p2 = Bare.of_string_exn E.Arg_value.decode s in
  let s2 = Bare.to_string E.Arg_value.encode p2 in
  Printf.printf "p2:\n%s\n" @@ Hex.hexdump_s (Hex.of_string s2);
  assert (s = s2);
  assert (a = p2);
  ()
