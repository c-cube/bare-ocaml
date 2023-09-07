module E = Example1
module Foo_ = Example1_pp (* just refer to it *)
module Bare = Bare_encoding

let () =
  print_endline ">>> run encoding test 1";
  let p1 =
    E.Person.Customer
      {
        E.Customer.name = "foo";
        email = "foo@bar.com";
        orders = [| { E.Customer_orders_0.orderId = 97L; quantity = 106l } |];
        metadata =
          Bare.String_map.singleton "mood" (Bytes.of_string "jolly good!");
        address =
          {
            E.Address.address = [| "123"; "lol road"; "so"; "far away" |];
            city = "Paris";
            state = "là bas";
            country = "Eurozone 51";
          };
      }
  in
  let s = Bare.to_string E.Person.encode p1 in
  (let oc = open_out "foo.data" in
   output_string oc s;
   flush oc;
   close_out oc);
  let p2 = Bare.of_string_exn E.Person.decode s in
  let s2 = Bare.to_string E.Person.encode p2 in
  (let oc = open_out "foo2.data" in
   output_string oc s2;
   flush oc;
   close_out oc);
  assert (s = s2);
  assert (p1 = p2);
  ()
