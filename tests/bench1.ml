module E = Example1
module Bare = Bare_encoding

let now = Unix.gettimeofday
let n = try Sys.getenv "N" |> int_of_string with _ -> 1_000_000
let coeff = try Sys.getenv "COEFF" |> int_of_string with _ -> 10
(* objects per string *)

let () =
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

  let encoded_p1 = Bare.to_string E.Person.encode p1 in
  let len = String.length encoded_p1 in
  Printf.printf
    "do %d iterations, %d objects each (each object is %d bytes)\n%!" n coeff
    len;

  (let t1 = now () in
   let buf = Buffer.create (len * 2) in
   for _i = 1 to n do
     Buffer.clear buf;
     let out = Bare.Encode.of_buffer buf in
     for _j = 1 to coeff do
       E.Person.encode out p1
     done
   done;

   let dur = now () -. t1 in
   let written = float (n * coeff * len) in
   Printf.printf "written %.3f GB in %.2fs (%.3fGB/s)\n%!" (written *. 1e-9) dur
     (written *. 1e-9 /. dur));

  (let str = String.concat "" (Array.make coeff encoded_p1 |> Array.to_list) in

   let t1 = now () in

   for _i = 1 to n do
     let dec = Bare.Decode.of_string str in

     for _j = 1 to coeff do
       let _p = E.Person.decode dec in
       ()
     done
   done;

   let dur = now () -. t1 in
   let read = float (n * len) in
   Printf.printf "read %.3f GB in %.2fs (%.3fGB/s)\n%!" (read *. 1e-9) dur
     (read *. 1e-9 /. dur));

  ()
