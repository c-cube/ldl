[@@@ocaml.alert "-unstable"]

open Common_

let run = Loop.run

module FD = struct
  type t = Unix.file_descr

  let dispose (self : t) =
    Effect.perform (Effects_.Close_fd self);
    Unix.close self

  let rec read fd buf i len : int =
    match Unix.read fd buf i len with
    | n -> n
    | exception Unix.Unix_error ((Unix.EWOULDBLOCK | Unix.EAGAIN), _, _) ->
      Effect.perform (Effects_.Wait_read fd);
      read fd buf i len

  let rec read_exact fd buf i len =
    if len > 0 then (
      let n = read fd buf i len in
      if n = 0 then raise End_of_file;
      read_exact fd buf (i + n) (len - n)
    )

  let rec write fd buf i len : int =
    match Unix.write fd buf i len with
    | n -> n
    | exception Unix.Unix_error ((Unix.EWOULDBLOCK | Unix.EAGAIN), _, _) ->
      Effect.perform (Effects_.Wait_write fd);
      write fd buf i len

  let write_all fd buf i len : unit =
    let i = ref i in
    let len = ref len in
    while !len > 0 do
      let n = write fd buf !i !len in
      i := !i + n;
      len := !len - n
    done

  let write_str fd s = write_all fd (Bytes.of_string s) 0 (String.length s)
  let writef fd fmt = Printf.ksprintf (write_str fd) fmt
end

let yield () : unit = Effect.perform Effects_.Yield

module File = struct
  let ret_nonblock fd =
    Unix.set_nonblock fd;
    fd

  let stdin () = ret_nonblock Unix.stdin
  let stdout () = ret_nonblock Unix.stdout
  let stderr () = ret_nonblock Unix.stderr

  let with_in path f =
    let fd = Unix.openfile path [ Unix.O_NONBLOCK; Unix.O_RDONLY ] 0o644 in
    let@ () = Fun.protect ~finally:(fun () -> FD.dispose fd) in
    f fd

  let with_out path f =
    let fd =
      Unix.openfile path [ Unix.O_CREAT; Unix.O_WRONLY; Unix.O_NONBLOCK ] 0o644
    in
    let@ () = Fun.protect ~finally:(fun () -> FD.dispose fd) in
    f fd
end

module Net = struct
  type sockaddr = Unix.sockaddr

  let listen_local ~port () : FD.t =
    let server_sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.bind server_sock (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
    Unix.listen server_sock 32;
    Unix.setsockopt_optint server_sock Unix.SO_LINGER None;
    Unix.setsockopt server_sock Unix.SO_REUSEPORT true;
    Unix.set_nonblock server_sock;
    server_sock

  let rec accept fd : Unix.file_descr * Unix.sockaddr =
    match Unix.accept fd with
    | sock, addr ->
      Unix.set_nonblock sock;
      sock, addr
    | exception Unix.Unix_error ((Unix.EWOULDBLOCK | Unix.EAGAIN), _, _) ->
      Effect.perform (Effects_.Wait_read fd);
      accept fd

  let with_connect host port f =
    let addr = Unix.inet_addr_of_string host in
    let sockaddr = Unix.ADDR_INET (addr, port) in
    let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.connect sock sockaddr;
    Unix.set_nonblock sock;
    let@ () = Fun.protect ~finally:(fun () -> FD.dispose sock) in
    f sock
end

module Fut = Fut

let spawn f = Fut.spawn f
