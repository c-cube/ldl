[@@@ocaml.alert "-unstable"]

open Common_

type 'a waiter = 'a or_error -> unit

type 'a state =
  | Done of 'a or_error
  | Waiting of 'a waiter list

type 'a t = { st: 'a state A.t } [@@unboxed]
type 'a promise = 'a t

let make () =
  let fut = { st = A.make (Waiting []) } in
  fut, fut

let of_result x : _ t = { st = A.make (Done x) }
let[@inline] return x : _ t = of_result (Ok x)
let[@inline] fail e bt : _ t = of_result (Error (e, bt))

let[@inline] is_resolved self : bool =
  match A.get self.st with
  | Done _ -> true
  | Waiting _ -> false

let[@inline] peek self : _ option =
  match A.get self.st with
  | Done x -> Some x
  | Waiting _ -> None

let[@inline] is_done self : bool =
  match A.get self.st with
  | Done _ -> true
  | Waiting _ -> false

exception Not_ready

let[@inline] get_or_fail self =
  match A.get self.st with
  | Done x -> x
  | Waiting _ -> raise Not_ready

let[@inline] get_or_fail_exn self =
  match A.get self.st with
  | Done (Ok x) -> x
  | Done (Error (exn, bt)) -> Printexc.raise_with_backtrace exn bt
  | Waiting _ -> raise Not_ready

let on_result (self : _ t) (f : _ waiter) : unit =
  while
    let st = A.get self.st in
    match st with
    | Done x ->
      f x;
      false
    | Waiting l ->
      let must_retry = not (A.compare_and_set self.st st (Waiting (f :: l))) in
      must_retry
  do
    Domain.cpu_relax ()
  done

exception Already_fulfilled

let fulfill (self : _ t) (r : _ result) : unit =
  while
    let st = A.get self.st in
    match st with
    | Done _ -> raise Already_fulfilled
    | Waiting l ->
      let did_swap = A.compare_and_set self.st st (Done r) in
      if did_swap then (
        (* success, now call all the waiters *)
        List.iter (fun f -> try f r with _ -> ()) l;
        false
      ) else
        true
  do
    Domain.cpu_relax ()
  done

let[@inline] fulfill_idempotent self r =
  try fulfill self r with Already_fulfilled -> ()

let await (self : 'a t) : 'a =
  match peek self with
  | Some (Ok x) -> x
  | Some (Error (exn, bt)) -> Printexc.raise_with_backtrace exn bt
  | None ->
    (* wait for completion *)
    Effect.perform
      (Effects_.Suspend
         (fun wakeup ->
           (* call [wakeup()] when the function is done *)
           on_result self (fun _ -> wakeup ())));
    get_or_fail_exn self

module Internal_ = struct
  let create f : 'a t * task =
    let res, promise = make () in
    (* task that computes [f()] and fulfills the future *)
    let run () =
      match f () with
      | x -> fulfill promise (Ok x)
      | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        fulfill promise (Error (exn, bt))
    in
    res, run
end

let spawn f : _ t =
  let res, run = Internal_.create f in
  Effect.perform (Effects_.Schedule run);
  res
