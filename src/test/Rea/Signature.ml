(* DO NOT EDIT! THIS IS AUTO EXTRACTED FROM THE SIGNATURE. *)

[@@@ocaml.warning "-32"] (* Disable unused value warning. *)

open Rea

let rec fib n =
  eta'0 @@ fun () ->
  if n <= 1 then
    pure n
  else
    lift'2 ( + ) (fib (n - 2)) (fib (n - 1))

let rec map_er xyE =
  eta'1 @@ function
  | [] -> pure []
  | x :: xs ->
    let+ y = xyE x and+ ys = map_er xyE xs in
    y :: ys
