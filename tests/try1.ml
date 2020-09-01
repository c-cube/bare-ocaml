
module E = Example1

let () =
  let p1 = E.Person.Customer
      {E.Customer. name="foo"; email="foo@bar.com";
       orders=[|{E.Customer_orders_0.orderId=31L; quantity=6l}|];
       metadata=Bare.String_map.singleton "mood" (Bytes.of_string "jolly good!");
       address={E.Address.address=[|"123"; "lol road"; "far away"|];
                city="Paris"; state="là bas"; country="Eurozone 51";
               }
      } in
  let s = Bare.to_string E.Person.encode p1 in
  let p2 = Bare.of_string_exn E.Person.decode s in
  assert (p1 = p2);
  ()

