module Fib = Ldl.Fiber

let epf = Printf.eprintf

let () =
  let port = ref 8085 in
  let debug = ref false in

  let opts =
    [ "-p", Arg.Set_int port, " port"; "-d", Arg.Set debug, " debug" ]
    |> Arg.align
  in
  Arg.parse opts ignore "";

  epf "listen on port %d\n%!" !port;
  let server_sock = Ldl.Net.listen_local ~port:!port () in

  Ldl.run @@ fun () ->
  while true do
    let client_sock, client_addr = Ldl.Net.accept server_sock in

    let client_ip, client_port =
      match client_addr with
      | Unix.ADDR_INET (ip, port) -> Unix.string_of_inet_addr ip, port
      | _ -> assert false
    in
    if !debug then epf "got client on %s:%d!\n%!" client_ip client_port;

    let _fib_client =
      Fib.spawn (fun () ->
          let buf = Bytes.create 128 in
          let continue = ref true in
          while !continue do
            let n = Ldl.FD.read client_sock buf 0 (Bytes.length buf) in
            if !debug then
              epf "got %d bytes from client %s:%d\n%!" n client_ip client_port;
            if n > 0 then
              Ldl.FD.write_all client_sock buf 0 n
            else
              continue := false
          done;
          if !debug then
            epf "client %s:%d disconnected\n%!" client_ip client_port)
    in
    ()
  done;

  ()
