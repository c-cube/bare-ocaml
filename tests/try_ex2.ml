
module E = Example2
module Bare = Bare_encoding

let () =
  print_endline ">>> run encoding test 2";
  let p1 = E.PTreeNode {
      left=E.PTreeNil;
      person={E.Person.first="jean"; last="valjean"};
      right=E.PTreeNode{
          left=E.PTreeNil;
          person={E.Person.first="something something"; last="javert";};
          right=E.PTreeNil;
        };
    }
  in
  let s = Bare.to_string E.PTree.encode p1 in
  begin
    let oc = open_out "try2_foo.data" in
    output_string oc s; flush oc;
    close_out oc;
  end;
  let p2 = Bare.of_string_exn E.PTree.decode s in
  let s2 = Bare.to_string E.PTree.encode p2 in
  begin
    let oc = open_out "try2_foo2.data" in
    output_string oc s2; flush oc;
    close_out oc;
  end;
  assert (s = s2);
  assert (p1 = p2);
  ()

