type 'a t = {
  m: Mutex.t;
  v: 'a;
}

let create v : _ t = { v; m = Mutex.create () }

let with_ (self : _ t) f =
  Mutex.lock self.m;
  try
    let x = f self.v in
    Mutex.unlock self.m;
    x
  with e ->
    Mutex.unlock self.m;
    raise e
