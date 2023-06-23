let epf = Printf.eprintf

let () =
  Ldl.run @@ fun () ->
  let stdin = Ldl.File.stdin () in
  let stdout = Ldl.File.stdout () in

  let buf = Bytes.create 32 in

  let continue = ref true in
  while !continue do
    let n = Ldl.FD.read stdin buf 0 (Bytes.length buf) in
    if n = 0 then
      continue := false
    else
      Ldl.FD.write_all stdout buf 0 n
  done;
  epf "done\n%!"
