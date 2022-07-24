open Signatures
open Util

let eta'0 f d = f () d
let eta'1 f x d = f x d
let eta'2 f x y d = f x y d

(* *)

let run d xE = xE d

(* *)

let env (d : (_, _) #pure') = d#pure' d
let env_as da (d : (_, _) #pure') = d#pure' (da d)
let mapping_env ds aE d = aE (ds d)
let setting_env s = mapping_env (const s)

(* *)

let get p = env_as @@ Prop.get p
let get_as p va = env_as (Prop.get p >>> va)
let mapping p f = mapping_env @@ Prop.map p f
let setting p v = mapping_env @@ Prop.set p v

(* *)

let map xy xE (d : (_, _) #map') = d#map' xy xE
let ( let+ ) xE xy = map xy xE
let ( >>- ) = ( let+ )
let ( >-> ) xyE yz x = eta'1 xyE x >>- yz
let lift'1 = map

(* *)

let pure x (d : (_, _) #pure') = d#pure' x
let unit (d : (_, _) #pure') = d#pure' ()
let return = pure
let do_unless c uM = if c then unit else uM
let do_when c uM = if c then uM else unit

(* *)

let pair xE yE (d : (_, _) #pair') = d#pair' xE yE
let ( and+ ) = pair
let ( <*> ) = ( and+ )
let tuple'2 = ( and+ )

let tuple'3 x1E x2E x3E =
  let+ x1 = x1E and+ x2 = x2E and+ x3 = x3E in
  (x1, x2, x3)

let tuple'4 x1E x2E x3E x4E =
  let+ x1 = x1E and+ x2 = x2E and+ x3 = x3E and+ x4 = x4E in
  (x1, x2, x3, x4)

let tuple'5 x1E x2E x3E x4E x5E =
  let+ x1 = x1E and+ x2 = x2E and+ x3 = x3E and+ x4 = x4E and+ x5 = x5E in
  (x1, x2, x3, x4, x5)

let tuple'6 x1E x2E x3E x4E x5E x6E =
  let+ x1 = x1E
  and+ x2 = x2E
  and+ x3 = x3E
  and+ x4 = x4E
  and+ x5 = x5E
  and+ x6 = x6E in
  (x1, x2, x3, x4, x5, x6)

let map_er'1 = eta'1
let map_er'2 f1 f2 (x1, x2) = tuple'2 (eta'1 f1 x1) (eta'1 f2 x2)

let map_er'3 f1 f2 f3 (x1, x2, x3) =
  tuple'3 (eta'1 f1 x1) (eta'1 f2 x2) (eta'1 f3 x3)

let map_er'4 f1 f2 f3 f4 (x1, x2, x3, x4) =
  tuple'4 (eta'1 f1 x1) (eta'1 f2 x2) (eta'1 f3 x3) (eta'1 f4 x4)

let map_er'5 f1 f2 f3 f4 f5 (x1, x2, x3, x4, x5) =
  tuple'5 (eta'1 f1 x1) (eta'1 f2 x2) (eta'1 f3 x3) (eta'1 f4 x4) (eta'1 f5 x5)

let map_er'6 f1 f2 f3 f4 f5 f6 (x1, x2, x3, x4, x5, x6) =
  tuple'6 (eta'1 f1 x1) (eta'1 f2 x2) (eta'1 f3 x3) (eta'1 f4 x4) (eta'1 f5 x5)
    (eta'1 f6 x6)

let map_eq_er'1 = map_er'1

let map_eq_er'2 f1 f2 ((x1, x2) as x) =
  let+ y1 = f1 x1 and+ y2 = f2 x2 in
  if x1 == y1 && x2 == y2 then x else (y1, y2)

let map_eq_er'3 f1 f2 f3 ((x1, x2, x3) as x) =
  let+ y1 = f1 x1 and+ y2 = f2 x2 and+ y3 = f3 x3 in
  if x1 == y1 && x2 == y2 && x3 == y3 then x else (y1, y2, y3)

let map_eq_er'4 f1 f2 f3 f4 ((x1, x2, x3, x4) as x) =
  let+ y1 = f1 x1 and+ y2 = f2 x2 and+ y3 = f3 x3 and+ y4 = f4 x4 in
  if x1 == y1 && x2 == y2 && x3 == y3 && x4 == y4 then x else (y1, y2, y3, y4)

let map_eq_er'5 f1 f2 f3 f4 f5 ((x1, x2, x3, x4, x5) as x) =
  let+ y1 = f1 x1
  and+ y2 = f2 x2
  and+ y3 = f3 x3
  and+ y4 = f4 x4
  and+ y5 = f5 x5 in
  if x1 == y1 && x2 == y2 && x3 == y3 && x4 == y4 && x5 == y5 then x
  else (y1, y2, y3, y4, y5)

let map_eq_er'6 f1 f2 f3 f4 f5 f6 ((x1, x2, x3, x4, x5, x6) as x) =
  let+ y1 = f1 x1
  and+ y2 = f2 x2
  and+ y3 = f3 x3
  and+ y4 = f4 x4
  and+ y5 = f5 x5
  and+ y6 = f6 x6 in
  if x1 == y1 && x2 == y2 && x3 == y3 && x4 == y4 && x5 == y5 && x6 == y6 then x
  else (y1, y2, y3, y4, y5, y6)

let lift'2 x1x2y x1 x2 = x1 <*> x2 >>- fun (x1, x2) -> x1x2y x1 x2

let lift'3 x1x2x3y x1 x2 x3 =
  x1 <*> x2 <*> x3 >>- fun ((x1, x2), x3) -> x1x2x3y x1 x2 x3

let lift'4 x1x2x3x4y x1 x2 x3 x4 =
  x1 <*> x2 <*> x3 <*> x4 >>- fun (((x1, x2), x3), x4) -> x1x2x3x4y x1 x2 x3 x4

let lift'5 x1x2x3x4x5y x1 x2 x3 x4 x5 =
  x1 <*> x2 <*> x3 <*> x4 <*> x5 >>- fun ((((x1, x2), x3), x4), x5) ->
  x1x2x3x4x5y x1 x2 x3 x4 x5

let lift'6 x1x2x3x4x5x6y x1 x2 x3 x4 x5 x6 =
  x1 <*> x2 <*> x3 <*> x4 <*> x5 <*> x6
  >>- fun (((((x1, x2), x3), x4), x5), x6) -> x1x2x3x4x5x6y x1 x2 x3 x4 x5 x6

(* *)

let branch xzE yzE xyE (d : (_, _) #branch') = d#branch' xzE yzE xyE
let true' = `Fst ()
let false' = `Snd ()
let bool' = function true -> true' | false -> false'
let if_else_s tE eE cE = branch (map const tE) (map const eE) (map bool' cE)

(* *)

let bind xE xyE (d : (_, _) #bind') = d#bind' xE xyE
let ( let* ) = bind
let ( >>= ) = bind
let ( and* ) = ( and+ )
let join xEE = bind xEE id
let ( >> ) uE xE = bind uE (const xE)
let ( >=> ) abE bcE a = bind (eta'1 abE a) bcE
let ( &&& ) lE rE = bind lE (fun l -> if l then rE else pure false)
let ( ||| ) lE rE = bind lE (fun l -> if l then pure true else rE)

(* *)

let zero (d : (_, _) #zero') = d#zero'

(* *)

let alt xE yE (d : (_, _) #alt') = d#alt' xE yE
let ( <|> ) = alt
let rec iota i m d = if i = m then pure i d else (pure i <|> iota (i + 1) m) d
let iota n = if n <= 0 then zero else iota 0 (n - 1)
let filter p xs = xs >>= fun x -> if p x then pure x else zero

(* *)

let fail e (d : (_, _) #fail') = d#fail' e
let tryin exE yxE yE (d : (_, _) #tryin') = d#tryin' exE yxE yE
let catch xF = tryin (error >>> pure) (ok >>> pure) xF
let handle exE = tryin exE pure
let finally uE = tryin (fail >>> ( >> ) uE) (pure >>> ( >> ) uE)
let map_error fe = tryin (fe >>> fail) return
let gen_error xE = map_error (function (_ : nothing) -> .) xE

(* *)

let par xE yE (d : (_, _) #par') = d#par' xE yE
let suspend on (d : (_, _) #suspend') = d#suspend' on
let spawn uE (d : (_, _) #spawn') = d#spawn' uE
