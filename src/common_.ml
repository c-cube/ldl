module A = Atomic

type task = unit -> unit
type 'a or_error = ('a, exn * Printexc.raw_backtrace) result
