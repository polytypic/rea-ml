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
    Traverse.to_exists Seq.map_er (( = ) 3)
      (Seq.of_rea
         (map
            (fun x ->
              incr n_calls;
              x + 1)
            (iota 1000000000000000) Seq.monad_plus)));
  assert (3 = !n_calls)

let () =
  let n_calls = ref 0 in
  assert (
    Traverse.to_exists Seq.map_er (( = ) 3)
      (Seq.of_rea
         (( iota 1000000000000000 >>= fun x ->
            incr n_calls;
            pure (x + 1) )
            Seq.monad_plus)));
  assert (3 = !n_calls)
