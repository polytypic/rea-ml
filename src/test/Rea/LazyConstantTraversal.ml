open Rea

module List : sig
  include module type of Stdlib.List
  include module type of StdRea.List
end = struct
  include Stdlib.List
  include StdRea.List
end

module Option : sig
  include module type of Stdlib.Option
  include module type of StdRea.Option
end = struct
  include Stdlib.Option
  include StdRea.Option
end

let () =
  assert (
    "101"
    = (Constant.from "101"
      |> map (( + ) 1)
      |> run Constant.functr |> Constant.of_rea))

let () =
  assert (Some 3 = (Some 1 |> Traverse.to_map Option.map_er @@ fun x -> x + 2))

let () =
  let n_calls = ref 0 in
  assert (
    true
    = ([3; 1; 4; 1; 5; 9; 2]
      |> Traverse.to_exists List.map_er @@ fun x ->
         incr n_calls;
         x = 4));
  assert (!n_calls = 3)

let () =
  let n_calls = ref 0 in
  assert (
    Some 4
    = ([3; 1; 4; 1; 5; 9; 2]
      |> Traverse.to_find_map List.map_er @@ fun x ->
         incr n_calls;
         if x = 4 then
           Some 4
         else
           None));
  assert (!n_calls = 3)

let () =
  let n_calls = ref 0 in
  assert (
    Some 4
    = ([3; 1; 4; 1; 5; 9; 2]
      |> Traverse.to_find_map_er List.map_er (fun x ->
             incr n_calls;
             if x = 4 then
               pure @@ Some 4
             else
               pure @@ None)
      |> run Identity.monad |> Identity.of_rea));
  assert (!n_calls = 3)

let () =
  let map_er' nE o1E o2E iE eE =
    eta'1 @@ function
    | `Num x -> map_er'1 nE x >>- fun x -> `Num x
    | `Uop x -> map_er'2 o1E eE x >>- fun x -> `Uop x
    | `Bop x -> map_er'3 o2E eE eE x >>- fun x -> `Bop x
    | `Lam x -> map_er'2 iE eE x >>- fun x -> `Lam x
    | `App x -> map_er'2 eE eE x >>- fun x -> `App x
    | `Var x -> map_er'1 iE x >>- fun x -> `Var x
  in
  let map_er eE = map_er' pure pure pure pure eE in
  let n_calls = ref 0 in
  let rec is_free i' e =
    incr n_calls;
    match e with
    | `Var i -> i = i'
    | `Lam (i, _) when i = i' -> false
    | e -> Traverse.to_exists map_er (is_free i') e
  in
  assert (is_free "f" (`App (`Var "f", `Var "x")));
  assert (!n_calls = 2)
