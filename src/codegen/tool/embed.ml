
let () =
  let code =
    let ic = open_in Sys.argv.(1) in

    let b = Bytes.create 128 in
    let buf = Buffer.create 128 in
    let continue = ref true in

    while !continue do
      let n = input ic b 0 (Bytes.length b) in
      if n = 0 then continue := false
      else Buffer.add_subbytes buf b 0 n
    done;
    Buffer.contents buf
  in

  Printf.printf "let code = %S\n%!" code
