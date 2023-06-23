(** LDL.

    Loop-dee-loop, an event loop for OCaml 5.
*)

module Fiber = Fiber

val run : (unit -> 'a) -> 'a
(** [run f] runs [f()] in a new event loop. When [f()] returns, the loop
    terminates. Inside [f()], effects, fibers, etc. can be used. *)

val yield : unit -> unit

module FD : sig
  type t = Unix.file_descr

  val read : t -> bytes -> int -> int -> int
  val read_exact : t -> bytes -> int -> int -> unit
  val write : t -> bytes -> int -> int -> int
  val write_str : t -> string -> unit
  val writef : t -> ('a, unit, string, unit) format4 -> 'a
  val write_all : t -> bytes -> int -> int -> unit
end

module File : sig
  val stdin : unit -> FD.t
  val stdout : unit -> FD.t
  val stderr : unit -> FD.t
  val with_in : string -> (FD.t -> 'a) -> 'a
  val with_out : string -> (FD.t -> 'a) -> 'a
end

module Net : sig
  type sockaddr = Unix.sockaddr

  val listen_local : port:int -> unit -> FD.t
  val accept : FD.t -> FD.t * sockaddr
  val with_connect : string -> int -> (FD.t -> 'a) -> 'a
end
