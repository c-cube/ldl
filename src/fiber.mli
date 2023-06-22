(** A fiber, returning a value of type ['a] *)

open Common_

type 'a t

val res : 'a t -> 'a Fut.t

val spawn : (unit -> 'a) -> 'a t
(** [spawn f] spawns a new fiber evaluating [f()] *)

(**/**)

module Internal_ : sig
  val create : (unit -> 'a) -> 'a t * task
end

(**/**)
