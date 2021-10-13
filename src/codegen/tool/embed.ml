
let () =
  let code =
    let ic = open_in Sys.argv.(1) in
    let len = in_channel_length ic in
    let buf = Bytes.create len in
    really_input ic buf 0 len;
    Bytes.unsafe_to_string buf
  in

  Printf.printf "let code = %S\n%!" code
