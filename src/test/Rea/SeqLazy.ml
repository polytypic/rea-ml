open Rea

module Seq : sig
  include module type of Stdlib.Seq
  include module type of StdRea.Seq
end = struct
  include Stdlib.Seq
  include StdRea.Seq
end

let () =
  let n_calls = ref 0 in
  assert (
    (let+ x = iota 1000000000000000 in
     incr n_calls;
     x + 1)
    |> run Seq.monad_plus |> Seq.of_rea
    |> Traverse.to_exists Seq.map_er (( = ) 3));
  assert (3 = !n_calls)

let () =
  let n_calls = ref 0 in
  assert (
    (let* x = iota 1000000000000000 in
     incr n_calls;
     pure (x + 1))
    |> run Seq.monad_plus |> Seq.of_rea
    |> Traverse.to_exists Seq.map_er (( = ) 3));
  assert (3 = !n_calls)
