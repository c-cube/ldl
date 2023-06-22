module Fib = Ldl.Fiber

let epf = Printf.eprintf

let () =
  let port = try int_of_string (Sys.getenv "PORT") with _ -> 8085 in
  epf "listen on port %d\n%!" port;
  let server_sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind server_sock (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
  Unix.listen server_sock 32;
  Unix.setsockopt_optint server_sock Unix.SO_LINGER None;
  Unix.setsockopt server_sock Unix.SO_REUSEPORT true;
  Unix.set_nonblock server_sock;

  Ldl.run @@ fun () ->
  while true do
    let client_sock, client_addr = Ldl.accept server_sock in
    Unix.set_nonblock client_sock;
    let client_ip, client_port =
      match client_addr with
      | Unix.ADDR_INET (ip, port) -> Unix.string_of_inet_addr ip, port
      | _ -> assert false
    in
    epf "got client on %s:%d!\n%!" client_ip client_port;

    let _fib_client =
      Fib.spawn (fun () ->
          let buf = Bytes.create 128 in
          let continue = ref true in
          while !continue do
            let n = Ldl.read client_sock buf 0 (Bytes.length buf) in
            epf "got %d bytes from client %s:%d\n%!" n client_ip client_port;
            if n > 0 then
              Ldl.write_all client_sock buf 0 n
            else
              continue := false
          done;
          epf "client %s:%d disconnected\n%!" client_ip client_port)
    in
    ()
  done;

  ()
