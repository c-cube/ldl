[@@@ocaml.alert "-unstable"]

open Common_

type 'a t = { res: 'a Fut.t } [@@unboxed]

let[@inline] res self = self.res

let create f : 'a t * task =
  let res, promise = Fut.make () in

  (* task that computes [f()] and fulfills the future *)
  let run () =
    match f () with
    | x -> Fut.fulfill promise (Ok x)
    | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      Fut.fulfill promise (Error (exn, bt))
  in

  { res }, run

let spawn f : _ t =
  let res, run = create f in
  Effect.perform (Effects_.Schedule run);
  res

module Internal_ = struct
  let create = create
end