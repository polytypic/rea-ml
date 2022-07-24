open Signatures
open Combinators

type ('R, 'e, 'a) initial = {run : 'E 'D. ('R, 'E, ('e, 'a) res, 'D) er}

type ('R, 'e, 'a) state =
  [ `Initial of ('R, 'e, 'a) initial
  | `Empty of (('e, 'a) res -> unit) list
  | ('e, 'a) res ]

type ('R, 'e, 'a) t = ('R, 'e, 'a) state ref

let empty = `Empty []

let create (eaE : (_, _, _, 'D) er) : (_, _, _, 'D) er =
  let+ (d : 'D) = env in
  (ref (`Initial {run = (fun _ -> catch eaE d)}) :> (_, _, _) t)

let eval (eaM : (_, _, _) t) =
  eta'0 @@ fun () ->
  match !eaM with
  | `Ok x -> pure x
  | `Error e -> fail e
  | `Empty _ -> (
    suspend @@ fun resume ->
    match !eaM with
    | #res as res -> resume res
    | `Empty ws -> eaM := `Empty (resume :: ws)
    | _ -> failwith "Memo.eval")
  | `Initial {run} -> (
    eaM := empty;
    let* (res : (_, _) res) = run in
    match !eaM with
    | `Empty ws -> (
      eaM := (res :> (_, _, _) state);
      List.iter (fun resume -> resume res) ws;
      match res with `Ok x -> pure x | `Error e -> fail e)
    | _ -> failwith "Memo.eval")
