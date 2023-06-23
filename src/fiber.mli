(** A fiber, returning a value of type ['a] *)

open Common_

type 'a t

val spawn : (unit -> 'a) -> 'a t
(** [spawn f] spawns a new fiber evaluating [f()] *)

val join : 'a t -> 'a
val is_done : _ t -> bool

(**/**)

module Internal_ : sig
  val create : (unit -> 'a) -> 'a t * task
end

(**/**)
