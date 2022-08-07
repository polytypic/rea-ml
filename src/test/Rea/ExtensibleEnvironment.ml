open Rea

module Value = struct
  class d =
    object
      val mutable value = 0
      method value = Prop.make (fun () -> value) (fun v -> value <- v)
    end

  let p d = d#value
end

module Counter = struct
  class d =
    object
      val counter = ref 0
      method counter_get = !counter
      method counter_inc = incr counter
    end

  let get d = env_as (fun d -> d#counter_get) d
  let inc d = env_as (fun d -> d#counter_inc) d
end

let rec until n =
  let* v = get Value.p in
  do_when (v < n) (Counter.inc >> setting Value.p (v + 1) (until n))

let () =
  assert (
    5
    = (until 5 >> Counter.get
      |> run
           (object
              inherit [_] Identity.monad
              inherit Counter.d
              inherit Value.d
           end)
      |> Identity.of_rea))

let () =
  assert (
    `Ok 5
    = (until 5 >> Counter.get
      |> Tailrec.run
           (object
              inherit [_] Tailrec.sync
              inherit Counter.d
              inherit Value.d
           end)))
