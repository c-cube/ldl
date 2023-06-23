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
  micro_q: task Queue.t Lock_.t;  (** Tasks to run immediately (micro-queue) *)
  main_task: 'a Fut.t;
}

type 'a t = {
  workers: 'a worker array;  (** offset 0: main worker *)
  off: int A.t;  (** round robin *)
}
[@@ocaml.warning "-69"]

let enqueue_task (self : _ t) (f : task) : unit =
  let n = A.fetch_and_add self.off 1 in
  (* pick which worker we enqueue this into *)
  let w = self.workers.(n mod Array.length self.workers) in
  let@ q = Lock_.with_ w.micro_q in
  Queue.push f q

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
let try_to_wakeup_sub_read (self : _ worker) fd : unit =
  match Fd_tbl.find_opt self.fd_tbl fd with
  | None | Some { readers = []; _ } -> ()
  | Some ({ readers = l; _ } as subs) ->
    List.iter (fun k -> enqueue_task self (fun () -> E.continue k ())) l;
    subs.readers <- [];
    update_poll_event self fd subs

(** Try to have one of the write subscribers do a write *)
let try_to_wakeup_sub_write (self : _ worker) fd : unit =
  match Fd_tbl.find_opt self.fd_tbl fd with
  | None | Some { writers = []; _ } -> ()
  | Some ({ writers = l; _ } as subs) ->
    List.iter (fun k -> enqueue_task self (fun () -> E.continue k ())) l;
    subs.writers <- [];
    update_poll_event self fd subs

let rec with_handler self f x =
  E.try_with f x
    {
      E.effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Effects_.Yield ->
            Some
              (fun (k : (a, _) E.continuation) ->
                enqueue_task self (E.continue k))
          | Effects_.Schedule f ->
            Some
              (fun (k : (a, _) E.continuation) ->
                (* schedule [f] and resume the current computation *)
                enqueue_task_with_handler self f;
                E.continue k ())
          | Effects_.Wait_read fd ->
            Some
              (fun (k : (a, _) E.continuation) -> add_read_sub_for_fd self fd k)
          | Effects_.Wait_write fd ->
            Some
              (fun (k : (a, _) E.continuation) ->
                add_write_sub_for_fd self fd k)
          | Effects_.Suspend f ->
            Some
              (fun (k : (a, _) E.continuation) ->
                let k' () = enqueue_task self (fun () -> E.continue k ()) in
                f k')
          | Effects_.Close_fd fd ->
            Some
              (fun (k : (a, _) E.continuation) ->
                (* TODO: cancel/discontinue waiters *)
                Fd_tbl.remove self.fd_tbl fd;
                E.continue k ())
          | Effects_.Exit ->
            if Atomic.exchange self.active false then Poll.close self.poll;
            None
          | _ -> None);
    }

and enqueue_task_with_handler (self : _ t) f : unit =
  enqueue_task self (fun () -> with_handler self f ())

(** run immediate tasks *)
let run_microtasks (self : _ worker) =
  let local_q = Queue.create () in
  while
    (* get tasks into [local_q] *)
    (let@ q = Lock_.with_ self.micro_q in
     Queue.transfer q local_q);

    if Queue.is_empty local_q then
      false
    else (
      (* execute tasks we just grabbed *)
      Queue.iter
        (fun task ->
          try task ()
          with e ->
            let bt = Printexc.get_raw_backtrace () in
            Printf.eprintf "uncaught exceptions: %s\n%s\n%!"
              (Printexc.to_string e)
              (Printexc.raw_backtrace_to_string bt))
        local_q;
      true
    )
  do
    ()
  done

let poll (self : _ worker) : unit =
  (* now poll *)
  if Fd_tbl.length self.fd_tbl = 0 then
    (* nothing more to do.
       TODO: fail the future, if present *)
    Atomic.set self.active false
  else (
    (* TODO: compute actual timeout if we have a heap/timer wheel *)
    let timeout = Poll.Timeout.never in
    match Poll.wait self.poll timeout with
    | `Timeout -> ()
    | `Ok ->
      (* gather all newly ready tasks *)
      let todo = ref [] in
      Poll.iter_ready self.poll ~f:(fun fd (ev : Poll.Event.t) ->
          todo := (fd, ev) :: !todo);
      Poll.clear self.poll;

      (* now wake them up *)
      List.iter
        (fun ((fd, ev) : _ * Poll.Event.t) ->
          if ev.readable then try_to_wakeup_sub_read self fd;
          if ev.writable then try_to_wakeup_sub_write self fd)
        !todo
  )

let main_loop (self : _ t) (self : _ worker) : unit =
  while Atomic.get self.active && not (Fut.is_done self.main_task) do
    run_microtasks self;
    poll self
  done

let create main_task : _ t =
  let active = A.make true in

  let mk_worker () =
    {
      active;
      poll = Poll.create ();
      fd_tbl = Fd_tbl.create 128;
      micro_q = Lock_.create @@ Queue.create ();
      main_task;
    }
  in

  let j = Domain.recommended_domain_count () - 1 in

  let main_worker = mk_worker () in
  let workers = Array.init j (fun _ -> mk_worker ()) in
  { main_worker; workers; off = A.make 0 }

let run (f : unit -> 'a) : 'a =
  let fiber, run = Fut.Internal_.create f in
  let self = create fiber in
  enqueue_task_with_handler self.main_worker run;

  let domains =
    Array.map
      (fun worker -> Domain.spawn (fun () -> main_loop self worker))
      self.workers
  in
  main_loop self self.main_worker;
  Array.iter Domain.join domains;
  Fut.await self.main_worker.main_task
