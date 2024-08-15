module E = Example1
module Bare = Bare_encoding

let now = Unix.gettimeofday
(* objects per string *)

let () =
  let n = ref 1_000_000 in
  let batch = ref 8 in
  let repeat = ref 5 in

  let opts =
    Arg.align
      [
        "-n", Arg.Set_int n, " number of iterations";
        "--batch", Arg.Set_int batch, " batch size";
        "--repeat", Arg.Set_int repeat, " repeat size";
      ]
  in
  Arg.parse opts ignore "";

  let p1 =
    E.Person.Customer
      {
        E.Customer.name = "foo";
        email = "foo@bar.com";
        orders = [| { E.Customer_orders_0.orderId = 97L; quantity = 106l } |];
        metadata =
          Bare.String_map.singleton "mood" (Bytes.of_string "jolly good!");
        address =
          [|
            [| "123"; "lol road"; "so"; "far away" |];
            [| "Paris" |];
            [| "là bas" |];
            [| "Eurozone 51" |];
          |];
      }
  in

  let persons = Array.make !batch p1 in
  let encoded_l = Bare.to_string E.Persons.encode persons in
  let len = String.length encoded_l in
  Printf.printf
    "do %d iterations with %d persons each, %d repetitions each (each list of \
     persons is %d bytes)\n\
     %!"
    !n !batch !repeat len;

  (let t1 = now () in
   let buf = Buffer.create (len * 2) in
   for _i = 1 to !n do
     Buffer.clear buf;
     let out = Bare.Encode.of_buffer buf in
     for _j = 1 to !repeat do
       E.Persons.encode out persons
     done
   done;

   let dur = now () -. t1 in
   let written = float (!n * !repeat * len) in
   Printf.printf "written %.3f GB in %.2fs (%.3fGB/s)\n%!" (written *. 1e-9) dur
     (written *. 1e-9 /. dur));

  (let str = String.concat "" (Array.make !repeat encoded_l |> Array.to_list) in

   let t1 = now () in

   for _i = 1 to !n do
     let dec = Bare.Decode.of_string str in

     for _j = 1 to !repeat do
       let _p = E.Persons.decode dec in
       ()
     done
   done;

   let dur = now () -. t1 in
   let read = float (!n * !repeat * len) in
   Printf.printf "read %.3f GB in %.2fs (%.3fGB/s)\n%!" (read *. 1e-9) dur
     (read *. 1e-9 /. dur));

  ()
