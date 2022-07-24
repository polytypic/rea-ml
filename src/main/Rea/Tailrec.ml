open Signatures
open Combinators
open Derived
open Util

type r

type (+'e, +'a) t =
  | Pure : 'a -> ('e, 'a) t
  | Bind : ('e, 'b) t * ('b -> ('e, 'a) t) -> ('e, 'a) t
  | Fail : 'e -> ('e, 'a) t
  | Tryin : ('f -> ('e, 'a) t) * ('b -> ('e, 'a) t) * ('f, 'b) t -> ('e, 'a) t
  | Suspend : (('e, 'a) res, unit) cps -> ('e, 'a) t
  | Spawn : (unit -> 'a) -> ('e, 'a) t

type work = W : ('d -> (nothing, unit) t) * 'd -> work

let work = ref []
let running = ref false

let rec pop () =
  match !work with
  | W (xuT, x) :: ws ->
    work := ws;
    run (xuT x)
  | [] -> running := false

and push (W (xuT, x) as w) =
  if !running then work := w :: !work
  else (
    running := true;
    run (xuT x))

and run = function
  | Pure () -> pop ()
  | Fail (_ : nothing) -> .
  | Tryin (eyT, xyT, xT) -> (
    let tryin txT t = Tryin (eyT, xyT, txT t) in
    match xT with
    | Pure x -> run (xyT x)
    | Bind (zT, zxT) -> run (Tryin (eyT, tryin zxT, zT))
    | Fail e -> run (eyT e)
    | Tryin (exT, zxT, zT) -> run (Tryin (tryin exT, tryin zxT, zT))
    | Spawn spawn -> run (xyT (spawn ()))
    | Suspend s ->
      pop
        (s (function
          | `Ok x -> push (W (xyT, x))
          | `Error e -> push (W (eyT, e)))))
  | Bind (xT, xyT) -> (
    let bind txT t = Bind (txT t, xyT) in
    match xT with
    | Pure x -> run (xyT x)
    | Bind (zT, zxT) -> run (Bind (zT, bind zxT))
    | Fail _ -> .
    | Tryin (exT, zxT, zT) -> run (Tryin (bind exT, bind zxT, zT))
    | Spawn spawn -> run (xyT (spawn ()))
    | Suspend s ->
      pop (s (function `Ok x -> push (W (xyT, x)) | `Error _ -> .)))
  | Suspend on -> pop (on (function `Ok () -> () | `Error _ -> .))
  | Spawn spawn -> pop (spawn ())

external to_rea : ('e, 'a) t -> (r, 'e, 'a) s = "%identity"
external of_rea : (r, 'e, 'a) s -> ('e, 'a) t = "%identity"

class ['D] base =
  let cont xyE d x = of_rea (xyE x d) in
  object (d : 'D)
    inherit [r, 'D] monad'd
    method pure' x = to_rea (Pure x)
    method bind' xE xyE = to_rea (Bind (of_rea (xE d), cont xyE d))
    inherit [r, 'D] errors'
    method fail' e = to_rea (Fail e)

    method tryin' exE yxE yE =
      to_rea (Tryin (cont exE d, cont yxE d, of_rea (yE d)))
  end

class ['D] sync =
  object (d : 'D)
    inherit ['D] base
    method suspend' = d
  end

let sync = new sync

let run (d : 'D #sync as 'D) xE =
  let result = ref None in
  run (of_rea ((catch xE >>- (Option.some >>> ( := ) result)) (d :> _)));
  Option.get !result

class ['D] async =
  object (d : 'D)
    inherit ['D] base
    inherit [r, 'D] par'd
    method suspend' s = to_rea (Suspend s)
    method spawn' nuE = to_rea (Spawn (fun () -> push (W (nuE >>> of_rea, d))))
  end

let async = new async
let spawn d uE = push (W (uE >>> of_rea, d))
