open Signatures
open Derived
open Combinators
open Util

module List = struct
  open List

  [%%if ocaml_version < (4, 10, 0)]

  let concat_map f l =
    let rec aux f acc = function
      | [] -> rev acc
      | x :: l ->
        let xs = f x in
        aux f (rev_append xs acc) l
    in
    aux f [] l

  [%%endif]

  let rec map_er xyE =
    eta'1 @@ function
    | [] -> pure []
    | x :: xs -> lift'2 cons (xyE x) (map_er xyE xs)

  let map_er xyE = map_er (eta'1 xyE)

  let rec map_eq_er xyE =
    eta'1 @@ function
    | [] -> pure []
    | x :: xs as xxs ->
      xyE x <*> map_eq_er xyE xs >>- fun (y, ys) ->
      if x == y && xs == ys then xxs else y :: ys

  let map_eq_er xyE = map_eq_er (eta'1 xyE)

  type r

  external to_rea : 'a t -> (r, 'e, 'a) s = "%identity"
  external of_rea : (r, 'e, 'a) s -> 'a t = "%identity"

  class ['D] monad_plus =
    object (d : 'D)
      inherit [r, 'D] monad'd
      method pure' x = to_rea [x]

      method bind' xE xyE =
        to_rea (concat_map (fun x -> of_rea (xyE x d)) (of_rea (xE d)))

      inherit [r, 'D] plus'
      method zero' = to_rea []
      method alt' lE rE = to_rea (append (of_rea (lE d)) (of_rea (rE d)))
    end

  let monad_plus = new monad_plus
end

module Option = struct
  let map_er xyE =
    eta'1 @@ function None -> pure None | Some x -> xyE x >>- Option.some

  let map_eq_er xyE =
    eta'1 @@ function
    | None -> pure None
    | Some x as xs -> xyE x >>- fun y -> if x == y then xs else Some y

  type r

  external to_rea : 'a Option.t -> (r, 'e, 'a) s = "%identity"
  external of_rea : (r, 'e, 'a) s -> 'a Option.t = "%identity"

  class ['D] monad_plus =
    object (d : 'D)
      inherit [r, 'D] monad'd
      method pure' x = to_rea (Some x)

      method bind' xE xyE =
        to_rea (Option.bind (of_rea (xE d)) (fun x -> of_rea (xyE x d)))

      inherit [r, 'D] plus'
      method zero' = to_rea None

      method alt' lE rE =
        match of_rea (lE d) with None -> rE d | some -> to_rea some
    end

  let monad_plus = new monad_plus
end

module Seq = struct
  open Seq

  [%%if ocaml_version < (4, 11, 0)]

  let cons x xs = const (Cons (x, xs))

  let rec append seq1 seq2 () =
    match seq1 () with
    | Nil -> seq2 ()
    | Cons (x, next) -> Cons (x, append next seq2)

  [%%endif]

  let rec map_er xyE =
    eta'1 @@ fun xs ->
    match xs () with
    | Nil -> pure empty
    | Cons (x, xs) -> lift'2 cons (xyE x) (map_er xyE xs)

  let map_er xyE = map_er (eta'1 xyE)

  type r

  external to_rea : 'a t -> (r, 'e, 'a) s = "%identity"
  external of_rea : (r, 'e, 'a) s -> 'a t = "%identity"

  class ['D] monad_plus =
    object (d : 'D)
      inherit [r, 'D] monad'd
      method pure' x = to_rea (return x)

      method bind' xE xyE =
        to_rea
          (Seq.flat_map
             (fun x () -> of_rea (xyE x d) ())
             (fun () -> of_rea (xE d) ()))

      inherit [r, 'D] plus'
      method zero' = to_rea empty

      method alt' lE rE =
        to_rea
          (append (fun () -> of_rea (lE d) ()) (fun () -> of_rea (rE d) ()))
    end

  let monad_plus = new monad_plus
  let run d xR = of_rea (xR d)
end

module Result = struct
  type r

  external to_rea : ('a, 'e) Result.t -> (r, 'e, 'a) s = "%identity"
  external of_rea : (r, 'e, 'a) s -> ('a, 'e) Result.t = "%identity"

  class ['D] monad_errors =
    object (d : 'D)
      inherit [r, 'D] monad'd
      method pure' x = to_rea (Ok x)

      method bind' xE xyE =
        to_rea (Result.bind (of_rea (xE d)) (fun x -> of_rea (xyE x d)))

      inherit [r, 'D] errors'
      method fail' e = to_rea (Error e)

      method tryin' eyE xyE xE =
        Result.fold ~ok:(flip xyE d) ~error:(flip eyE d) (of_rea (xE d))
    end

  let monad_errors = new monad_errors
end
