[@@@ocaml.alert "-unstable"]

type _ Effect.t +=
  | Read : Unix.file_descr * bytes * int * int -> int Effect.t
  | Write : Unix.file_descr * bytes * int * int -> int Effect.t
  | Accept : Unix.file_descr -> (Unix.file_descr * Unix.sockaddr) Effect.t
  | Yield : unit Effect.t
  | Schedule : (unit -> unit) -> unit Effect.t
