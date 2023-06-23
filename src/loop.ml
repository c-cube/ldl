[@@@ocaml.alert "-unstable"]

open Common_
module E = Effect.Deep

module Fd_tbl = Hashtbl.Make (struct
  type t = Unix.file_descr

  let equal : t -> t -> bool = ( = )
  let hash = Hashtbl.hash
end)

type suspended_reader = (unit, unit) E.continuation
type suspended_writer = (unit, unit) E.continuation

module Fd_subscribers = struct
  type t = {
    mutable readers: suspended_reader list;
    mutable writers: suspended_writer list;
  }

  let empty : t = { readers = []; writers = [] }
  let is_empty self = self.readers = [] && self.writers = []

  let to_event self =
    { Poll.Event.readable = self.readers <> []; writable = self.writers <> [] }
end

type 'a worker = {
  active: bool Atomic.t;
  poll: Poll.t;  (** Main polling structure *)
  fd_tbl: Fd_subscribers.t Fd_tbl.t;  (** Subscribers for a given FD *)
  micro_q: task Bb_queue.t;  (** Tasks to run immediately (micro-queue) *)
  main_task: 'a Fut.t;
}

type 'a t = {
  j: int;
  workers: 'a worker array;  (** offset 0: main worker *)
  off: int A.t;  (** round robin *)
}
[@@ocaml.warning "-69"]

let[@inline] enqueue_task_on (self : _ worker) (f : task) =
  Bb_queue.push self.micro_q f

(** pick a worker *)
let pick_worker_ (self : _ t) : _ worker =
  let n = A.fetch_and_add self.off 1 in
  let w = self.workers.(n mod Array.length self.workers) in
  w

let enqueue_task_any (self : _ t) (f : task) : unit =
  let n = A.fetch_and_add self.off 1 in
  (* pick which worker we enqueue this into *)
  let w = self.workers.(n mod Array.length self.workers) in
  enqueue_task_on w f

let update_poll_event (self : _ worker) fd (subs : Fd_subscribers.t) =
  if Fd_subscribers.is_empty subs then
    Fd_tbl.remove self.fd_tbl fd
  else
    Poll.set self.poll fd (Fd_subscribers.to_event subs)

let get_or_create_subs (self : _ worker) fd : Fd_subscribers.t =
  try Fd_tbl.find self.fd_tbl fd
  with Not_found ->
    let sub = Fd_subscribers.empty in
    Fd_tbl.add self.fd_tbl fd sub;
    sub

let add_read_sub_for_fd (self : _ worker) fd k : unit =
  let fd_subs = get_or_create_subs self fd in
  fd_subs.readers <- k :: fd_subs.readers;
  update_poll_event self fd fd_subs

let add_write_sub_for_fd (self : _ worker) fd k : unit =
  let fd_subs = get_or_create_subs self fd in
  fd_subs.writers <- k :: fd_subs.writers;
  update_poll_event self fd fd_subs

(** Try to have one of the read subscribers do a read *)
let try_to_wakeup_sub_read (_self : _ t) (w : _ worker) fd : unit =
  match Fd_tbl.find_opt w.fd_tbl fd with
  | None | Some { readers = []; _ } -> ()
  | Some ({ readers = l; _ } as subs) ->
    List.iter (fun k -> enqueue_task_on w (fun () -> E.continue k ())) l;
    subs.readers <- [];
    update_poll_event w fd subs

(** Try to have one of the write subscribers do a write *)
let try_to_wakeup_sub_write (_self : _ t) (w : _ worker) fd : unit =
  match Fd_tbl.find_opt w.fd_tbl fd with
  | None | Some { writers = []; _ } -> ()
  | Some ({ writers = l; _ } as subs) ->
    List.iter (fun k -> enqueue_task_on w (fun () -> E.continue k ())) l;
    subs.writers <- [];
    update_poll_event w fd subs

let rec with_handler (self : _ t) w f x =
  E.try_with f x
    {
      E.effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Effects_.Yield ->
            Some
              (fun (k : (a, _) E.continuation) ->
                enqueue_task_any self (E.continue k))
          | Effects_.Schedule f ->
            Some
              (fun (k : (a, _) E.continuation) ->
                (* schedule [f] and resume the current computation *)
                enqueue_task_with_handler self f;
                E.continue k ())
          | Effects_.Wait_read fd ->
            Some (fun (k : (a, _) E.continuation) -> add_read_sub_for_fd w fd k)
          | Effects_.Wait_write fd ->
            Some
              (fun (k : (a, _) E.continuation) -> add_write_sub_for_fd w fd k)
          | Effects_.Suspend f ->
            Some
              (fun (k : (a, _) E.continuation) ->
                let k' () = enqueue_task_on w (fun () -> E.continue k ()) in
                f k')
          | Effects_.Close_fd fd ->
            Some
              (fun (k : (a, _) E.continuation) ->
                (* TODO: cancel/discontinue waiters *)
                Fd_tbl.remove w.fd_tbl fd;
                E.continue k ())
          | Effects_.Exit ->
            if Atomic.exchange w.active false then Poll.close w.poll;
            None
          | _ -> None);
    }

and enqueue_task_with_handler (self : _ t) f : unit =
  let w = pick_worker_ self in
  enqueue_task_on w (fun () -> with_handler self w f ())

(** run immediate tasks *)
let run_microtasks ~block self (w : _ worker) =
  let local_q = Queue.create () in
  while
    (* get tasks into [local_q] *)
    Bb_queue.pop_all ~block w.micro_q local_q;

    if Queue.is_empty local_q then
      false
    else (
      (* execute tasks we just grabbed *)
      Queue.iter
        (fun task ->
          try with_handler self w task ()
          with e ->
            let bt = Printexc.get_raw_backtrace () in
            Printf.eprintf "uncaught exceptions: %s\n%s\n%!"
              (Printexc.to_string e)
              (Printexc.raw_backtrace_to_string bt))
        local_q;
      Queue.clear local_q;
      true
    )
  do
    ()
  done

let poll (self : _ t) (w : _ worker) : unit =
  (* now poll *)
  if Fd_tbl.length w.fd_tbl = 0 then
    (* nothing to poll, just wait for fibers *)
    run_microtasks ~block:true self w
  else (
    (* TODO: compute actual timeout if we have a heap/timer wheel *)
    let timeout = Poll.Timeout.never in
    match Poll.wait w.poll timeout with
    | `Timeout -> ()
    | `Ok ->
      (* gather all newly ready tasks *)
      let todo = ref [] in
      Poll.iter_ready w.poll ~f:(fun fd (ev : Poll.Event.t) ->
          todo := (fd, ev) :: !todo);
      Poll.clear w.poll;

      (* now wake them up *)
      List.iter
        (fun ((fd, ev) : _ * Poll.Event.t) ->
          if ev.readable then try_to_wakeup_sub_read self w fd;
          if ev.writable then try_to_wakeup_sub_write self w fd)
        !todo
  )

let main_loop (self : _ t) (w : _ worker) : unit =
  while Atomic.get w.active && not (Fut.is_done w.main_task) do
    run_microtasks ~block:false self w;
    poll self w
  done

let create main_task : _ t =
  let active = A.make true in

  let mk_worker () =
    {
      active;
      poll = Poll.create ();
      fd_tbl = Fd_tbl.create 128;
      micro_q = Bb_queue.create ();
      main_task;
    }
  in

  let j = Domain.recommended_domain_count () - 1 in

  let workers = Array.init (j + 1) (fun _ -> mk_worker ()) in
  { j; workers; off = A.make 0 }

let run (f : unit -> 'a) : 'a =
  let fiber, run = Fut.Internal_.create f in
  let self = create fiber in
  enqueue_task_with_handler self run;

  let domains =
    Array.init self.j (fun i ->
        let w = self.workers.(i + 1) in
        Domain.spawn (fun () -> main_loop self w))
  in
  main_loop self self.workers.(0);
  Array.iter Domain.join domains;
  Fut.await self.workers.(0).main_task
