open Signatures
open Combinators

type 'c r

external to_rea : 'c -> ('c r, 'e, 'a) s = "%identity"
external of_rea : ('c r, 'e, 'a) s -> 'c = "%identity"

let from x _ = to_rea x

(* *)

let map d _ xC = to_rea (of_rea (xC d))

class ['c, 'D] functr =
  object (d : 'D)
    inherit ['c r, 'D] map'
    method map' = map d
  end

let functr =
  object (d)
    method map' : 'e 'a 'b. ('c r, 'e, 'a, 'b, 'D) map'm = map d
  end

let pair_with d combine xC yC =
  to_rea (combine (fun () -> of_rea (xC d)) (fun () -> of_rea (yC d)))

class ['c, 'D] product combine =
  object (d : 'D)
    inherit ['c, 'D] functr
    inherit ['c r, 'D] pair'
    method pair' = pair_with d combine
  end

let pure_of x _ = to_rea x

class ['c, 'D] applicative identity combine =
  object (_ : 'D)
    inherit ['c, 'D] product combine
    inherit ['c r, 'D] pure'
    method pure' = pure_of identity
  end

(* *)

let conjunction = new applicative true @@ fun l r -> l () && r ()
let disjunction = new applicative false @@ fun l r -> l () || r ()

let option =
  object (d : 'D)
    method map' : 'e 'a 'b. (_, 'e, 'a, 'b, 'D) map'm = map d
    method pure' : 'e 'a. (_, 'e, 'a, 'D) pure'm = pure_of None

    method pair' : 'e 'a 'b. (_, 'e, 'a, 'b, 'D) pair'm =
      pair_with d @@ fun l r -> match l () with None -> r () | s -> s
  end

let unit_er =
  object (d : 'D)
    method map' : 'e 'a 'b. (_, 'e, 'a, 'b, 'D) map'm = map d
    method pure' : 'e 'a. (_, 'e, 'a, 'D) pure'm = pure_of unit

    method pair' : 'e 'a 'b. (_, 'e, 'a, 'b, 'D) pair'm =
      pair_with d @@ fun l r -> l () >>= r
  end

let false_er d = pure false d
let true_er d = pure true d

let disjunction_er =
  object (d : 'D)
    method map' : 'e 'a 'b. (_, 'e, 'a, 'b, 'D) map'm = map d
    method pure' : 'e 'a. (_, 'e, 'a, 'D) pure'm = pure_of false_er

    method pair' : 'e 'a 'b. (_, 'e, 'a, 'b, 'D) pair'm =
      pair_with d @@ fun l r ->
      l () >>= function true -> true_er | false -> r ()
  end

let conjunction_er =
  object (d : 'D)
    method map' : 'e 'a 'b. (_, 'e, 'a, 'b, 'D) map'm = map d
    method pure' : 'e 'a. (_, 'e, 'a, 'D) pure'm = pure_of true_er

    method pair' : 'e 'a 'b. (_, 'e, 'a, 'b, 'D) pair'm =
      pair_with d @@ fun l r ->
      l () >>= function false -> false_er | true -> r ()
  end

let none_er d = pure None d

let option_er =
  object (d : 'D)
    method map' : 'e 'a 'b. (_, 'e, 'a, 'b, 'D) map'm = map d
    method pure' : 'e 'a. (_, 'e, 'a, 'D) pure'm = pure_of none_er

    method pair' : 'e 'a 'b. (_, 'e, 'a, 'b, 'D) pair'm =
      pair_with d @@ fun l r ->
      l () >>= function None -> r () | some -> pure some
  end
