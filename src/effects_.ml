[@@@ocaml.alert "-unstable"]

type _ Effect.t +=
  | Wait_read : Unix.file_descr -> unit Effect.t
  | Wait_write : Unix.file_descr -> unit Effect.t
  | Close_fd : Unix.file_descr -> unit Effect.t
  | Yield : unit Effect.t
  | Schedule : (unit -> unit) -> unit Effect.t
  | Suspend : ((unit -> unit) -> unit) -> unit Effect.t
  | Exit
