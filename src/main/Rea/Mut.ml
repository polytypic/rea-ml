open Signatures
open Combinators
open Util

type 'v t = [`Empty of ('v ok -> unit) list | 'v ok] ref

let create x = ref @@ `Ok x
let empty = `Empty []

let wait xMV =
  suspend @@ fun resume ->
  match !xMV with
  | `Ok _ as ok ->
    xMV := empty;
    resume ok
  | `Empty rs -> xMV := `Empty ((resume :> _ ok -> _) :: rs)

let take xMV =
  eta'0 @@ fun () ->
  match !xMV with
  | `Ok x ->
    xMV := empty;
    pure x
  | `Empty _ -> wait xMV

let fill xMV x =
  let ok = `Ok x in
  match !xMV with
  | `Empty [] -> xMV := ok
  | `Empty (resume :: rs) ->
    xMV := `Empty rs;
    resume ok
  | `Ok _ -> failwith "Mut.fill"

let read xMV =
  eta'0 @@ fun () ->
  match !xMV with
  | `Ok x -> pure x
  | `Empty _ ->
    let+ x = wait xMV in
    fill xMV x;
    x

let mutate fn xMV = map (fn >>> fill xMV) (take xMV)

let modify xya xMV =
  let+ x = take xMV in
  let x, a = xya x in
  fill xMV x;
  a

let try_mutate xxE xMV =
  let* x = take xMV in
  eta'1 xxE x
  |> tryin
       (fun e ->
         fill xMV x;
         fail e)
       (fun x ->
         fill xMV x;
         pure ())

let try_modify xxaE xMV =
  let* x = take xMV in
  eta'1 xxaE x
  |> tryin
       (fun e ->
         fill xMV x;
         fail e)
       (fun (x, a) ->
         fill xMV x;
         pure a)

module Syntax = struct
  let read p = get p >>= read
  let mutate p xx = get p >>= mutate xx
  let modify p xxa = get p >>= modify xxa
  let try_mutate p xxE = get p >>= try_mutate xxE
  let try_modify p xxaE = get p >>= try_modify xxaE
  let cloning p xE = read p >>= fun v -> setting p (create v) xE
end
