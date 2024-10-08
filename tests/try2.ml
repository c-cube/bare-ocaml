module E = Example1
module Bare = Bare_encoding

let () =
  print_endline ">>> run encoding test 2";
  let p1 =
    E.Person.Employee
      {
        E.Employee.name = "bar";
        email = "bar@iHaveAVeryLongEmailRight.edu.ac.co.uk";
        department = E.Department.CUSTOMER_SERVICE;
        hireDate = "2020-04-01";
        publicKey = Some (Bytes.make 128 'A');
        metadata =
          Bare.String_map.singleton "level" (Bytes.of_string "triple A grade!");
        address =
          [|
            [| "12"; "business avenue"; "front"; "center" |];
            [| "Business City" |];
            [| "HQ" |];
            [| "Office Space 1" |];
          |];
      }
  in
  let s = Bare.to_string E.Person.encode p1 in
  (let oc = open_out "bar.data" in
   output_string oc s;
   flush oc;
   close_out oc);
  Printf.printf "p1:\n%s\n" @@ Hex.hexdump_s (Hex.of_string s);
  let p2 = Bare.of_string_exn E.Person.decode s in
  let s2 = Bare.to_string E.Person.encode p2 in
  Printf.printf "p2:\n%s\n" @@ Hex.hexdump_s (Hex.of_string s2);
  (let oc = open_out "bar2.data" in
   output_string oc s2;
   flush oc;
   close_out oc);
  assert (s = s2);
  assert (p1 = p2);
  ()
