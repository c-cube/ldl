type t

val create : unit -> t
val run : t -> (unit -> 'a) -> 'a
val close : t -> unit
