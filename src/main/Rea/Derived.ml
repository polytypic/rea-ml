open Signatures
open Combinators
open Util

class virtual ['R, 'D] monad'd =
  let map xy xE = xE >>= fun x -> pure (xy x)
  and pair xE yE =
    xE >>= fun x ->
    yE >>- fun y -> (x, y)
  and branch xzE yzE xyE =
    xyE >>= function
    | `Fst x -> xzE >>- fun xz -> xz x
    | `Snd y -> yzE >>- fun yz -> yz y
  in
  object (d : 'D)
    inherit ['R, 'D] monad'
    method map' xy xE = map xy xE d
    method pair' xE yE = pair xE yE d
    method branch' xzE yzE xyE = branch xzE yzE xyE d
  end

class virtual ['R, 'D] par'd =
  let par xE yE =
    unit >>= fun () ->
    let resume = ref None in
    let xR = ref `None and yR = ref `None in
    let check () =
      match (!resume, !xR, !yR) with
      | Some r, `Ok x, `Ok y -> r (`Ok (x, y))
      | Some r, (`Error _ as e), (`Ok _ | `Error _)
      | Some r, `Ok _, (`Error _ as e) ->
        r e
      | _, _, _ -> ()
    in
    map (( := ) xR >>> check) (catch xE)
    >> map (( := ) yR >>> check) (catch yE)
    >> suspend (Option.some >>> ( := ) resume >>> check)
  in
  object (d : 'D)
    inherit ['R, 'D] monad'
    inherit ['R, 'D] tryin'
    inherit ['R, 'D] suspend'
    inherit ['R, 'D] spawn'
    inherit ['R, 'D] par'
    method par' xE yE = par xE yE d
  end
