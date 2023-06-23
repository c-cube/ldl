[@@@ocaml.alert "-unstable"]

open Common_
module E = Effect.Deep

module Fd_tbl = Hashtbl.Make (struct
  type t = Unix.file_descr

  let equal : t -> t -> bool = ( = )
  let hash = Hashtbl.hash
end)

type suspended_reader =
  | Read of (int, unit) E.continuation * bytes * int * int
  | Accept of (Unix.file_descr * Unix.sockaddr, unit) E.continuation

type suspended_writer = (int, unit) E.continuation * bytes * int * int

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

type t = {
  active: bool Atomic.t;
  poll: Poll.t;  (** Main polling structure *)
  fd_tbl: Fd_subscribers.t Fd_tbl.t;  (** Subscribers for a given FD *)
  q: task Queue.t;  (** Tasks to run immediately (micro-queue) *)
}

let create () : t =
  {
    active = Atomic.make true;
    poll = Poll.create ();
    fd_tbl = Fd_tbl.create 128;
    q = Queue.create ();
  }

let close self = if Atomic.exchange self.active false then Poll.close self.poll
let[@inline] enqueue_task (self : t) (f : task) : unit = Queue.push f self.q

let update_poll_event (self : t) fd (subs : Fd_subscribers.t) =
  if Fd_subscribers.is_empty subs then
    Fd_tbl.remove self.fd_tbl fd
  else
    Poll.set self.poll fd (Fd_subscribers.to_event subs)

let get_or_create_subs (self : t) fd : Fd_subscribers.t =
  try Fd_tbl.find self.fd_tbl fd
  with Not_found ->
    let sub = Fd_subscribers.empty in
    Fd_tbl.add self.fd_tbl fd sub;
    sub

let add_read_sub_for_fd (self : t) fd k : unit =
  let fd_subs = get_or_create_subs self fd in
  fd_subs.readers <- k :: fd_subs.readers;
  update_poll_event self fd fd_subs

let add_write_sub_for_fd (self : t) fd k : unit =
  let fd_subs = get_or_create_subs self fd in
  fd_subs.writers <- k :: fd_subs.writers;
  update_poll_event self fd fd_subs

let try_read_once_ fd buf i len =
  try
    let n = Unix.read fd buf i len in
    Some n
  with Unix.Unix_error ((Unix.EWOULDBLOCK | Unix.EAGAIN), _, _) -> None

let try_write_once_ fd buf i len =
  try
    let n = Unix.write fd buf i len in
    Some n
  with Unix.Unix_error ((Unix.EWOULDBLOCK | Unix.EAGAIN), _, _) -> None

let try_accept fd =
  match Unix.accept fd with
  | sock, addr -> Some (sock, addr)
  | exception Unix.Unix_error ((Unix.EWOULDBLOCK | Unix.EAGAIN), _, _) -> None

let do_read (self : t) k fd buf i len : unit =
  try
    match try_read_once_ fd buf i len with
    | Some n -> E.continue k n
    | None -> add_read_sub_for_fd self fd @@ Read (k, buf, i, len)
  with e -> E.discontinue k e

let do_write (self : t) k fd buf i len : unit =
  try
    match try_write_once_ fd buf i len with
    | Some n -> E.continue k n
    | None -> add_write_sub_for_fd self fd (k, buf, i, len)
  with e -> E.discontinue k e

let do_accept (self : t) k fd : unit =
  try
    match try_accept fd with
    | Some (sock, addr) -> E.continue k (sock, addr)
    | None -> add_read_sub_for_fd self fd @@ Accept k
  with e -> E.discontinue k e

(** Try to have one of the read subscribers do a read *)
let try_to_wakeup_sub_read (self : t) fd : unit =
  match Fd_tbl.find_opt self.fd_tbl fd with
  | None | Some { readers = []; _ } -> ()
  | Some ({ readers = r0 :: tl; _ } as subs) ->
    (match r0 with
    | Accept k ->
      (match try_accept fd with
      | Some sock ->
        subs.readers <- tl;
        update_poll_event self fd subs;
        enqueue_task self (fun () -> E.continue k sock)
      | None -> ()
      | exception e ->
        subs.readers <- tl;
        update_poll_event self fd subs;
        E.discontinue k e)
    | Read (k, buf, i, len) ->
      (match try_read_once_ fd buf i len with
      | Some n ->
        subs.readers <- tl;
        update_poll_event self fd subs;
        enqueue_task self (fun () -> E.continue k n)
      | None -> (* still registered *) ()
      | exception e ->
        subs.readers <- tl;
        update_poll_event self fd subs;
        E.discontinue k e))

(** Try to have one of the write subscribers do a write *)
let try_to_wakeup_sub_write (self : t) fd : unit =
  match Fd_tbl.find_opt self.fd_tbl fd with
  | None | Some { writers = []; _ } -> ()
  | Some ({ writers = (k, buf, i, len) :: tl; _ } as subs) ->
    (match try_write_once_ fd buf i len with
    | Some n ->
      subs.writers <- tl;
      update_poll_event self fd subs;
      enqueue_task self (fun () -> E.continue k n)
    | None -> (* still registered *) ()
    | exception e ->
      subs.writers <- tl;
      update_poll_event self fd subs;
      E.discontinue k e)

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
                (* schedule [f] and resume *)
                enqueue_task_with_handler self f;
                E.continue k ())
          | Effects_.Read (fd, buf, i, len) ->
            Some
              (fun (k : (a, _) E.continuation) -> do_read self k fd buf i len)
          | Effects_.Write (fd, buf, i, len) ->
            Some
              (fun (k : (a, _) E.continuation) -> do_write self k fd buf i len)
          | Effects_.Accept fd ->
            Some (fun (k : (a, _) E.continuation) -> do_accept self k fd)
          | _ -> None);
    }

and enqueue_task_with_handler self f : unit =
  enqueue_task self (fun () -> with_handler self f ())

(** run immediate tasks *)
let run_microtasks (self : t) =
  while not (Queue.is_empty self.q) do
    let task = Queue.pop self.q in
    try task ()
    with e ->
      let bt = Printexc.get_raw_backtrace () in
      Printf.eprintf "uncaught exceptions: %s\n%s\n%!" (Printexc.to_string e)
        (Printexc.raw_backtrace_to_string bt)
  done

let poll (self : t) : unit =
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
      Poll.iter_ready self.poll ~f:(fun fd (ev : Poll.Event.t) ->
          if ev.readable then try_to_wakeup_sub_read self fd;
          if ev.writable then try_to_wakeup_sub_write self fd)
  )

let main_loop (self : t) : unit =
  while Atomic.get self.active do
    run_microtasks self;
    poll self
  done

let run (self : t) (f : unit -> 'a) : 'a =
  let _fiber, run = Fiber.Internal_.create f in
  enqueue_task_with_handler self run;
  main_loop self;
  assert false (* TODO: have main loop stop when [fiber] is done *)
