module F = Ldl.Fiber

let epf = Printf.eprintf

let () =
  Ldl.run @@ fun () ->
  Unix.set_nonblock Unix.stdin;
  Unix.set_nonblock Unix.stdout;

  let buf = Bytes.create 32 in

  let continue = ref true in
  while !continue do
    let n = Ldl.read Unix.stdin buf 0 (Bytes.length buf) in
    if n = 0 then
      continue := false
    else
      Ldl.write_all Unix.stdout buf 0 n
  done;
  epf "done\n%!"
