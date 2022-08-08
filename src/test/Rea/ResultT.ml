open Rea

module ResultT : sig
  type 'I r

  (*val to_rea : ('I, nothing, ('a, 'e) Result.t) s -> ('I r, 'e, 'a) s*)
  val of_rea : ('I r, 'e, 'a) s -> ('I, nothing, ('a, 'e) Result.t) s

  class ['I, 'D] monad_errors'on :
    ('I, 'M) #monad'
    -> object ('D)
         method map' : ('I r, 'e, 'a, 'b, 'D) map'm
         method pure' : ('I r, 'e, 'a, 'D) pure'm
         method pair' : ('I r, 'e, 'a, 'b, 'D) pair'm
         method branch' : ('I r, 'e, 'a, 'b, 'c, 'D) branch'm
         method bind' : ('I r, 'e, 'a, 'b, 'D) bind'm
         method fail' : ('I r, 'e, 'a, 'D) fail'm
         method tryin' : ('I r, 'e, 'f, 'a, 'b, 'D) tryin'm
       end
end = struct
  type 'I r

  external to_rea : ('I, nothing, ('a, 'e) Result.t) s -> ('I r, 'e, 'a) s
    = "%identity"

  external of_rea : ('I r, 'e, 'a) s -> ('I, nothing, ('a, 'e) Result.t) s
    = "%identity"

  class ['I, 'D] monad_errors'on (m : ('I, 'M) #monad') =
    object (d : 'D)
      inherit ['I r, 'D] monad'd
      method pure' x = to_rea (m#pure' (Ok x))

      method bind' xE xyE =
        to_rea
          (m#bind' (fun _ -> of_rea (xE d)) @@ function
           | Error e -> fun _ -> m#pure' (Error e)
           | Ok x -> fun _ -> of_rea (xyE x d))

      inherit ['I r, 'D] errors'
      method fail' e = to_rea (m#pure' (Error e))

      method tryin' eyE xyE xE =
        to_rea
          (m#bind' (fun _ -> of_rea (xE d)) @@ function
           | Error e -> fun _ -> of_rea (eyE e d)
           | Ok x -> fun _ -> of_rea (xyE x d))
    end
end

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
end

module Full = struct
  let rec eval = function
    | #Num.t as e -> Num.eval eval e
    | #Lam.t as e -> Lam.eval eval e

  let () =
    assert (
      Error (`Error_unbound_var "y")
      = (eval (`App (`Lam ("x", `Bop (`Add, `Num 2, `Var "y")), `Num 1))
        |> run
             (object
                inherit [_, _] ResultT.monad_errors'on Identity.monad
                inherit [_] Lam.bindings
             end)
        |> ResultT.of_rea |> Identity.of_rea))

  let () =
    assert (
      Ok (`Num 3)
      = (eval (`App (`Lam ("x", `Bop (`Add, `Num 2, `Var "x")), `Num 1))
        |> run
             (object
                inherit [_, _] ResultT.monad_errors'on Identity.monad
                inherit [_] Lam.bindings
             end)
        |> ResultT.of_rea |> Identity.of_rea))
end
