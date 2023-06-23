let ( let@ ) = ( @@ )
let epf = Printf.eprintf
let msg = "hello world\n"
let n_bytes_written = ref 0

let one_test ~host ~port () =
  let@ sock = Ldl.Net.with_connect host port in

  let buf = Bytes.create (String.length msg) in
  for _i = 1 to 10_000 do
    Ldl.FD.write_str sock msg;
    n_bytes_written := !n_bytes_written + String.length msg;
    Ldl.FD.read_exact sock buf 0 (String.length msg)
  done;
  ()

let run ~host ~port ~debug ~n_conn () : unit =
  let t0 = Unix.gettimeofday () in

  Ldl.run @@ fun () ->
  (* spawn fibers to connect *)
  let fibers =
    Array.init n_conn (fun _ ->
        Ldl.Fiber.spawn (fun () -> one_test ~host ~port ()))
  in

  Array.iteri
    (fun i fib ->
      Ldl.Fiber.join fib;
      if debug then epf "done with fib %d\n%!" i)
    fibers;

  let t1 = Unix.gettimeofday () in
  epf "all done (in %.3fs)\n%!" (t1 -. t0);
  epf "wrote %d bytes (%.2fB/s)\n%!" !n_bytes_written
    (float !n_bytes_written /. (t1 -. t0));
  ()

let () =
  let host = ref "127.0.0.1" in
  let port = ref 8085 in
  let n_conn = ref 500 in
  let debug = ref false in

  let opts =
    [
      "-d", Arg.Set debug, " debug";
      "-h", Arg.Set_string host, " host";
      "-p", Arg.Set_int port, " port";
      "--n-conn", Arg.Set_int n_conn, " number of connections";
    ]
    |> Arg.align
  in

  Arg.parse opts ignore "";
  run ~host:!host ~port:!port ~n_conn:!n_conn ~debug:!debug ()
