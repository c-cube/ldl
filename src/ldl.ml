(** LDL.

    Loop-dee-loop, an event loop for OCaml 5.
*)

[@@@ocaml.alert "-unstable"]

module Fiber = Fiber

(** [run f] runs [f()] in a new event loop. When [f()] returns, the loop
    terminates. Inside [f()], effects, fibers, etc. can be used. *)
let run (f : unit -> 'a) : 'a =
  let loop = Loop.create () in
  Loop.run loop f

let read fd buf i len : int = Effect.perform (Effects_.Read (fd, buf, i, len))
let write fd buf i len : int = Effect.perform (Effects_.Write (fd, buf, i, len))

let accept fd : Unix.file_descr * Unix.sockaddr =
  Effect.perform (Effects_.Accept fd)

let write_all fd buf i len : unit =
  let i = ref i in
  let len = ref len in
  while !len > 0 do
    let n = write fd buf !i !len in
    i := !i + n;
    len := !len - n
  done

let yield () : unit = Effect.perform Effects_.Yield
