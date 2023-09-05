
module E = Example3
module Bare = Bare_encoding

let () =
  print_endline ">>> run encoding test 3";
  let a = E.Int 42 in
  let s = Bare.to_string E.Arg.encode a in
  let p2 = Bare.of_string_exn E.Arg.decode s in
  let s2 = Bare.to_string E.Arg.encode p2 in
  assert (s = s2);
  assert (a = p2);
  ()
