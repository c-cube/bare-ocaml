(* generated from "example1.bare" using bare-codegen *)
[@@@ocaml.warning "-26-27"]
module Bare = Bare_encoding
module PublicKey = struct
  type t = bytes
  
  (** @raise Invalid_argument in case of error. *)
  let decode (dec: Bare.Decode.t) : t =
    Bare.Decode.data_of ~size:128 dec
  
  let encode (enc: Bare.Encode.t) (self: t) : unit =
    (assert (Bytes.length self=128); Bare.Encode.data_of ~size:128 enc self)
  
end

module Time = struct
  type t = string
  
  (** @raise Invalid_argument in case of error. *)
  let decode (dec: Bare.Decode.t) : t =
    Bare.Decode.string dec
  
  let encode (enc: Bare.Encode.t) (self: t) : unit =
    Bare.Encode.string enc self
  
end

module Department = struct
  type t =
    ACCOUNTING
    | ADMINISTRATION
    | CUSTOMER_SERVICE
    | DEVELOPMENT
    | JSMITH
  
  let to_int = function
    | ACCOUNTING -> 0L
    | ADMINISTRATION -> 1L
    | CUSTOMER_SERVICE -> 2L
    | DEVELOPMENT -> 3L
    | JSMITH -> 99L
    
  let of_int = function
    | 0L -> ACCOUNTING
    | 1L -> ADMINISTRATION
    | 2L -> CUSTOMER_SERVICE
    | 3L -> DEVELOPMENT
    | 99L -> JSMITH
    | x -> invalid_arg
      (Printf.sprintf "unknown enum member for Department.t: %Ld" x)
  
  (** @raise Invalid_argument in case of error. *)
  let decode (dec: Bare.Decode.t) : t =
    of_int (Bare.Decode.uint dec)
  
  let encode (enc: Bare.Encode.t) (self: t) : unit =
    Bare.Encode.uint enc (to_int self)
  
end

module Address = struct
  type t = string array array
  
  (** @raise Invalid_argument in case of error. *)
  let decode (dec: Bare.Decode.t) : t =
    Array.init 4
      (fun _ ->
       (let len = Bare.Decode.uint dec in
        if len>Int64.of_int Sys.max_array_length then invalid_arg "array too big";
        Array.init (Int64.to_int len) (fun _ -> Bare.Decode.string dec)))
  
  let encode (enc: Bare.Encode.t) (self: t) : unit =
    (assert (Array.length self = 4);
       Array.iter (fun xi ->
                   (let arr = xi in
                    Bare.Encode.uint enc (Int64.of_int (Array.length arr));
                    Array.iter (fun xi -> Bare.Encode.string enc xi) arr))
       self)
  
end

module Customer_orders_0 = struct
  type t = {
    orderId: int64;
    quantity: int32;
  }
  
  (** @raise Invalid_argument in case of error. *)
  let decode (dec: Bare.Decode.t) : t =
    let orderId = Bare.Decode.i64 dec in
    let quantity = Bare.Decode.i32 dec in
    {orderId; quantity; }
  
  let encode (enc: Bare.Encode.t) (self: t) : unit =
    begin
      Bare.Encode.i64 enc self.orderId;
      Bare.Encode.i32 enc self.quantity;
    end
  
end

module Customer = struct
  type t = {
    name: string;
    email: string;
    address: Address.t;
    orders: Customer_orders_0.t array;
    metadata: bytes Bare.String_map.t;
  }
  
  (** @raise Invalid_argument in case of error. *)
  let decode (dec: Bare.Decode.t) : t =
    let name = Bare.Decode.string dec in
    let email = Bare.Decode.string dec in
    let address = Address.decode dec in
    let orders =
      (let len = Bare.Decode.uint dec in
       if len>Int64.of_int Sys.max_array_length then invalid_arg "array too big";
       Array.init (Int64.to_int len) (fun _ -> Customer_orders_0.decode dec)) in
    let metadata =
      (let len = Bare.Decode.uint dec in
       if len>Int64.of_int max_int then invalid_arg "array too big";
       List.init (Int64.to_int len)
         (fun _ ->
          let k = Bare.Decode.string dec in
          let v = Bare.Decode.data dec in
          k,v)
       |> List.to_seq |> Bare.String_map.of_seq) in
    {name; email; address; orders; metadata; }
  
  let encode (enc: Bare.Encode.t) (self: t) : unit =
    begin
      Bare.Encode.string enc self.name;
      Bare.Encode.string enc self.email;
      Address.encode enc self.address;
      (let arr = self.orders in
       Bare.Encode.uint enc (Int64.of_int (Array.length arr));
       Array.iter (fun xi -> Customer_orders_0.encode enc xi) arr);
      (Bare.Encode.uint enc (Int64.of_int (Bare.String_map.cardinal self.metadata));
       Bare.String_map.iter
         (fun x y -> Bare.Encode.string enc x; Bare.Encode.data enc y)
         self.metadata);
    end
  
end

module Employee = struct
  type t = {
    name: string;
    email: string;
    address: Address.t;
    department: Department.t;
    hireDate: Time.t;
    publicKey: PublicKey.t option;
    metadata: bytes Bare.String_map.t;
  }
  
  (** @raise Invalid_argument in case of error. *)
  let decode (dec: Bare.Decode.t) : t =
    let name = Bare.Decode.string dec in
    let email = Bare.Decode.string dec in
    let address = Address.decode dec in
    let department = Department.decode dec in
    let hireDate = Time.decode dec in
    let publicKey =
      Bare.Decode.optional (fun dec -> PublicKey.decode dec) dec in
    let metadata =
      (let len = Bare.Decode.uint dec in
       if len>Int64.of_int max_int then invalid_arg "array too big";
       List.init (Int64.to_int len)
         (fun _ ->
          let k = Bare.Decode.string dec in
          let v = Bare.Decode.data dec in
          k,v)
       |> List.to_seq |> Bare.String_map.of_seq) in
    {name; email; address; department; hireDate; publicKey; metadata; }
  
  let encode (enc: Bare.Encode.t) (self: t) : unit =
    begin
      Bare.Encode.string enc self.name;
      Bare.Encode.string enc self.email;
      Address.encode enc self.address;
      Department.encode enc self.department;
      Time.encode enc self.hireDate;
      Bare.Encode.optional
        (fun enc xopt -> PublicKey.encode enc xopt) enc self.publicKey;
      (Bare.Encode.uint enc (Int64.of_int (Bare.String_map.cardinal self.metadata));
       Bare.String_map.iter
         (fun x y -> Bare.Encode.string enc x; Bare.Encode.data enc y)
         self.metadata);
    end
  
end

module Person = struct
  type t =
    | Customer of Customer.t
    | Employee of Employee.t
    | TerminatedEmployee
    
  
  (** @raise Invalid_argument in case of error. *)
  let decode (dec: Bare.Decode.t) : t =
    let tag = Bare.Decode.uint dec in
    match tag with
    | 0L -> Customer (Customer.decode dec)
    | 1L -> Employee (Employee.decode dec)
    | 2L -> TerminatedEmployee
    | _ -> invalid_arg (Printf.sprintf "unknown union tag Person.t: %Ld" tag)
    
  
  let encode (enc: Bare.Encode.t) (self: t) : unit =
    match self with
    | Customer x ->
      Bare.Encode.uint enc 0L;
      Customer.encode enc x
    | Employee x ->
      Bare.Encode.uint enc 1L;
      Employee.encode enc x
    | TerminatedEmployee ->
      Bare.Encode.uint enc 2L
    
    
end

module Persons = struct
  type t = Person.t array
  
  (** @raise Invalid_argument in case of error. *)
  let decode (dec: Bare.Decode.t) : t =
    (let len = Bare.Decode.uint dec in
     if len>Int64.of_int Sys.max_array_length then invalid_arg "array too big";
     Array.init (Int64.to_int len) (fun _ -> Person.decode dec))
  
  let encode (enc: Bare.Encode.t) (self: t) : unit =
    (let arr = self in
     Bare.Encode.uint enc (Int64.of_int (Array.length arr));
     Array.iter (fun xi -> Person.encode enc xi) arr)
  
end


