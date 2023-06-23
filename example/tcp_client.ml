let ( let@ ) = ( @@ )
let epf = Printf.eprintf

let run ~host ~port () : unit =
  Ldl.run @@ fun () ->
  let@ sock = Ldl.Net.with_connect host port in

  let stdin = Ldl.File.stdin () in
  let stdout = Ldl.File.stdout () in
  let continue = ref true in

  let _read_from_sock =
    Ldl.spawn (fun () ->
        let buf = Bytes.create 128 in
        while !continue do
          let n = Ldl.FD.read sock buf 0 (Bytes.length buf) in
          if n = 0 then
            continue := false
          else (
            Ldl.FD.writef stdout "got: ";
            Ldl.FD.write_all stdout buf 0 n
          )
        done)
  in

  let buf = Bytes.create 128 in
  while !continue do
    let n = Ldl.FD.read stdin buf 0 (Bytes.length buf) in
    if n = 0 then
      continue := false
    else
      Ldl.FD.write_all sock buf 0 n
  done;
  epf "main loop: exit\n%!";
  ()

let () =
  let host = ref "127.0.0.1" in
  let port = ref 8085 in

  let opts =
    [ "-h", Arg.Set_string host, " host"; "-p", Arg.Set_int port, " port" ]
    |> Arg.align
  in

  Arg.parse opts ignore "";
  run ~host:!host ~port:!port ()
