(* DO NOT EDIT! THIS IS AUTO EXTRACTED FROM THE README. *)

open Rea

let rec eager_fib n =
  if n <= 1 then
    pure n
  else
    lift'2 ( + ) (eager_fib (n - 2)) (eager_fib (n - 1))

let _ =
  (eager_fib
    : int ->
      (< map' : 'e 'a 'b. ('b -> 'a) -> ('R, 'e, 'b, 'D) er -> ('R, 'e, 'a) s
       ; pair' :
           'e 'a 'b.
           ('R, 'e, 'a, 'D) er -> ('R, 'e, 'b, 'D) er -> ('R, 'e, 'a * 'b) s
       ; pure' : 'e 'a. 'a -> ('R, 'e, 'a) s
       ; .. >
       as
       'D) ->
      ('R, 'e, int) s)

let _ =
  (eager_fib
    : int ->
      (< ('R, 'D) map' ; ('R, 'D) pair' ; ('R, 'D) pure' ; .. > as 'D) ->
      ('R, 'e, int) s)

let _ = (eager_fib : int -> (('R, 'D) #applicative' as 'D) -> ('R, 'e, int) s)
let _ = (eager_fib : int -> ('R, 'e, int, (('R, 'D) #applicative' as 'D)) er)
let () = assert (55 = Identity.of_rea (run Identity.monad (eager_fib 10)))
let () = assert (`Ok 55 = Tailrec.run Tailrec.sync (eager_fib 10))

let rec fib n =
  eta'0 @@ fun () ->
  if n <= 1 then
    pure n
  else
    lift'2 ( + ) (fib (n - 2)) (fib (n - 1))

let () = assert (55 = Identity.of_rea (run Identity.monad (fib 10)))
let () = assert (`Ok 55 = Tailrec.run Tailrec.sync (fib 10))

module Num = struct
  type 't t =
    [`Num of int | `Uop of [`Neg] * 't | `Bop of [`Add | `Mul] * 't * 't]

  let uop = function
    | `Neg -> ( ~-)

  let bop = function
    | `Add -> ( + )
    | `Mul -> ( * )

  let eval eval =
    eta'1 @@ function
    | `Num _ as v -> pure v
    | `Uop (op, x) -> (
      eval x >>= function
      | `Num x -> pure @@ `Num (uop op x)
      | x -> fail @@ `Error_attempt_to_apply_uop (op, x))
    | `Bop (op, l, r) -> (
      eval l <*> eval r >>= function
      | `Num l, `Num r -> pure @@ `Num (bop op l r)
      | l, r -> fail @@ `Error_attempt_to_apply_bop (op, l, r))

  let _ =
    (eval
      : ('t ->
        ( 'R,
          ([> `Error_attempt_to_apply_bop of
              ([< `Add | `Mul] as 'bop) * ([> `Num of int] as 'v) * 'v
           | `Error_attempt_to_apply_uop of ([< `Neg] as 'uop) * 'v ]
           as
           'e),
          'v,
          (< ('R, 'D) sync' ; .. > as 'D) )
        er) ->
        [< 't t] ->
        ('R, 'e, [> `Num of int], 'D) er)
end

module Lam = struct
  module Id = String

  type 't t = [`Lam of Id.t * 't | `App of 't * 't | `Var of Id.t]

  module Bindings = Map.Make (Id)

  class ['v] bindings :
    object
      method bindings : 'v Bindings.t Prop.t
    end =
    object
      val mutable v : 'v Bindings.t = Bindings.empty
      method bindings = Prop.make (fun () -> v) (fun x -> v <- x)
    end

  let bindings d = d#bindings

  let eval eval =
    eta'1 @@ function
    | `Lam (i, e) ->
      let+ bs = get bindings in
      `Fun (bs, i, e)
    | `App (f, x) -> (
      eval f >>= function
      | `Fun (bs, i, e) ->
        let* v = eval x in
        setting bindings (Bindings.add i v bs) (eval e)
      | f -> fail @@ `Error_attempt_to_apply f)
    | `Var i -> (
      get_as bindings (Bindings.find_opt i) >>= function
      | None -> fail @@ `Error_unbound_var i
      | Some v -> pure v)

  let _ =
    (eval
      : ('t ->
        ( 'R,
          ([> `Error_attempt_to_apply of
              ([> `Fun of 'v Bindings.t * Id.t * 't] as 'v)
           | `Error_unbound_var of Id.t ]
           as
           'e),
          'v,
          (< ('R, 'D) sync' ; 'v bindings ; .. > as 'D) )
        er) ->
        [< 't t] ->
        ('R, 'e, 'v, 'D) er)
end

module Full = struct
  let rec eval = function
    | #Num.t as e -> Num.eval eval e
    | #Lam.t as e -> Lam.eval eval e

  let _ =
    (eval
      : ([< 't Num.t | 't Lam.t] as 't) ->
        ( 'R,
          [> `Error_attempt_to_apply of
             ([> `Fun of 'v Lam.Bindings.t * Lam.Id.t * 't | `Num of int] as 'v)
          | `Error_attempt_to_apply_bop of [`Add | `Mul] * 'v * 'v
          | `Error_attempt_to_apply_uop of [`Neg] * 'v
          | `Error_unbound_var of Lam.Id.t ],
          'v,
          (< ('R, 'D) sync' ; 'v Lam.bindings ; .. > as 'D) )
        er)
end

let () =
  assert (
    Error (`Error_unbound_var "y")
    = StdRea.Result.of_rea
        (run
           (object
              inherit [_] StdRea.Result.monad_errors
              inherit [_] Lam.bindings
           end)
           (Full.eval
              (`App (`Lam ("x", `Bop (`Add, `Num 2, `Var "y")), `Num 1)))))

let () =
  assert (
    `Ok (`Num 3)
    = Tailrec.run
        (object
           inherit [_] Tailrec.sync
           inherit [_] Lam.bindings
        end)
        (Full.eval (`App (`Lam ("x", `Bop (`Add, `Num 2, `Var "x")), `Num 1))))

let () =
  let result = ref @@ Ok (`Num 0) in
  Tailrec.spawn
    (object
       inherit [_] Tailrec.async
       inherit [_] Lam.bindings
    end)
    (Full.eval (`App (`Lam ("x", `Bop (`Add, `Num 2, `Var "x")), `Num 1))
    |> tryin
         (fun e -> pure (result := Error e))
         (fun v -> pure (result := Ok v)));
  assert (!result = Ok (`Num 3))

module Cont : sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val callcc : (('a -> 'b t) -> 'a t) -> 'a t
  val run : 'a t -> 'a
end = struct
  type 'a t = ('a -> unit) -> unit

  let return x k = k x
  let bind xK xyK k = xK (fun x -> (xyK x) k)
  let callcc kxK k = kxK (fun x _ -> k x) k

  let run xK =
    let result = ref None in
    xK (fun x -> result := Some x);
    Option.get !result
end

class virtual ['R, 'D] callcc' =
  object
    method virtual callcc'
        : 'e 'f 'a 'b.
          (('a -> ('R, 'f, 'b, 'D) er) -> ('R, 'e, 'a, 'D) er) -> ('R, 'e, 'a) s
  end

let callcc f (d : (_, _) #callcc') = d#callcc' f

module ContRea = struct
  type r

  external to_rea : 'a Cont.t -> (r, 'e, 'a) s = "%identity"
  external of_rea : (r, 'e, 'a) s -> 'a Cont.t = "%identity"

  class ['D] monad_callcc =
    object (d : 'D)
      inherit [r, 'D] monad'd
      method pure' x = to_rea (Cont.return x)

      method bind' x f =
        to_rea (Cont.bind (of_rea (x d)) (fun x -> of_rea (f x d)))

      inherit [r, 'D] callcc'

      method callcc' f =
        to_rea (Cont.callcc (fun k -> of_rea (f (fun x _ -> to_rea (k x)) d)))
    end
end

let () =
  assert (
    55 = Cont.run (ContRea.of_rea (run (new ContRea.monad_callcc) (fib 10))))

let () =
  assert (
    101
    = Cont.run
        (ContRea.of_rea
           (run (new ContRea.monad_callcc) (callcc (fun k -> k 101)))))

module TheAnswer = struct
  let map_er' nE o1E o2E iE eE =
    eta'1 @@ function
    | `Num x -> map_er'1 nE x >>- fun x -> `Num x
    | `Uop x -> map_er'2 o1E eE x >>- fun x -> `Uop x
    | `Bop x -> map_er'3 o2E eE eE x >>- fun x -> `Bop x
    | `Lam x -> map_er'2 iE eE x >>- fun x -> `Lam x
    | `App x -> map_er'2 eE eE x >>- fun x -> `App x
    | `Var x -> map_er'1 iE x >>- fun x -> `Var x

  let map_er eE = map_er' pure pure pure pure eE

  type 't t = ['t Num.t | 't Lam.t]

  let _ =
    (map_er
      : ('s -> ('R, 'e, 't, (('R, 'D) #applicative' as 'D)) er) ->
        [< 's t] ->
        ('R, 'e, [> 't t], 'D) er)

  let rec is_free i' = function
    | `Var i -> i = i'
    | `Lam (i, _) when i = i' -> false
    | e -> Traverse.to_exists map_er (is_free i') e

  let () = assert (is_free "y" (`App (`Lam ("x", `Var "x"), `Var "y")))
  let () = assert (not (is_free "x" (`App (`Lam ("x", `Var "x"), `Var "y"))))
end
