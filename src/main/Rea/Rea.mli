(** {b WARNING}: Many of the combinators are documented concisely in the format
    {i "[X] is equivalent to [Y]"}.  The equivalence should only be assumed to
    hold modulo more or less obvious OCaml evaluation order differences. *)

(** {1 Reference} *)

(** {2 Type abbreviations} *)

type 'a ok = [`Ok of 'a]
(** Success type abbreviation. *)

type 'e error = [`Error of 'e]
(** Failure type abbreviation. *)

type ('e, 'a) res = ['a ok | 'e error]
(** Result type abbreviation. *)

type ('l, 'r) branch = [`Fst of 'l | `Snd of 'r]
(** Choice type abbrevation. *)

type ('d, 'c) cps = ('d -> 'c) -> 'c
(** Continuation passing style function type abbreviation. *)

type 'a op'1 = 'a -> 'a
(** Unary operation type abbreviation. *)

type 'a op'2 = 'a -> 'a -> 'a
(** Binary operation type abbreviation. *)

type 'a lazy_op'2 = (unit -> 'a) -> (unit -> 'a) -> 'a
(** Lazy binary operation type abbrevation. *)

(** {2 Abstract effect framework}

    The effect framework in this section is almost entirely free of concrete
    implementation details and should theoretically be usable in a wide variety
    of contexts including for wrapping existing monadic libraries.

    The effect framework can also be extended by users.  Consider the {!map}
    effect.  It consists of

    - the {!map'm} effect signature type abbreviation,
    - the {!map'} capability mix-in,
    - the {!map} combinator, and
    - the derived implementation in {!monad'd}.

    And, of course, various interpreters implement the {!map'} capability.  The
    bottom line is that nothing prevents user defined extensions following the
    same pattern. *)

(** Empty variant type used to ensure that no errors can be left unhandled. *)
type nothing = |

type ('R, +'e, +'a) s
(** Abstract effect [s]ignature represents the application [('e, 'a) 'R] of the
    {{:https://github.com/ocamllabs/higher} higher-kinded} effect representation
    type constructor ['R] to the error ['e] and answer ['a] types.

    Basic use of this framework should rarely require one to refer to this type,
    but this type will appear in inferred types.  When writing type signatures
    the effect reader type abbreviation {!type-er} should be preferred. *)

type ('R, 'e, 'a, 'D) er = 'D -> ('R, 'e, 'a) s
(** Effect reader takes a dictionary ['D] of capabilities and returns an effect
    with the signature [('R, 'e, 'a) s]. *)

(** {3 Laziness}

    Effect readers are functions that take a dictionary of capabilities.  This
    allows a form of laziness via η-expansion and effect signatures are designed
    to allow implementations to delay invoking the effect reader functions until
    the effects are really needed. *)

val eta'0 : (unit -> 'D -> 'a) -> 'D -> 'a
(** [eta'0 @@ fun () -> f] is equivalent to [fun d -> f d].

    Consider the following [fib] implementation:

    {[
        let rec fib n = eta'0 @@ fun () ->
          if n <= 1 then
            pure n
          else
            lift'2 (+) (fib (n - 2)) (fib (n - 1))
    ]}

    The [eta'0 @@ fun () -> ...] makes it so that [fib n] returns in O(1) time
    without building the complete computation tree. *)

val eta'1 : ('b1 -> 'd -> 'a) -> 'b1 -> 'd -> 'a
(** [eta'1 @@ fun x -> f] is equivalent to [fun x d -> f x d].

    Consider the following list traversal implementation:

    {[
        let rec map_er xyE = eta'1 @@ function
          | [] -> pure []
          | x :: xs ->
            let+ y = xyE x
            and+ ys = map_er xyE xs in
            y :: ys
    ]}

    The [eta'1 @@ function ...] makes it so that [map_er xyE xs] returns in O(1)
    time without going through the whole list to compute a complete computation
    tree for it. *)

val eta'2 : ('b1 -> 'b2 -> 'd -> 'a) -> 'b1 -> 'b2 -> 'd -> 'a
(** TODO *)

(** {3 Running} *)

val run : 'd -> ('d -> 'a) -> 'a
(** [run d xE] is equivalent to [xE d]. *)

(** {3 Functors} *)

type ('R, 'e, 'a, 'b, 'D) map'm =
  ('b -> 'a) -> ('R, 'e, 'b, 'D) er -> ('R, 'e, 'a) s
(** {!map} effect signature. *)

(** {!map} capability mix-in. *)
class virtual ['R, 'D] map' :
  object
    method virtual map' : ('R, 'e, 'a, 'b, 'D) map'm
    (** Implements the {!map} capability. *)
  end

(** Functor offers the {!map} capability. *)
class virtual ['R, 'D] functr' :
  object
    inherit ['R, 'D] map'
  end

val map :
  ('b -> 'a) -> ('R, 'e, 'b, (('R, 'D) #map' as 'D)) er -> ('R, 'e, 'a, 'D) er
(** [map xy xE] effect. *)

val ( let+ ) :
  ('R, 'e, 'b, (('R, 'D) #map' as 'D)) er -> ('b -> 'a) -> ('R, 'e, 'a, 'D) er
(** [( let+ ) xE xy] is equivalent to [map xy xE]. *)

val ( >>- ) :
  ('R, 'e, 'b, (('R, 'D) #map' as 'D)) er -> ('b -> 'a) -> ('R, 'e, 'a, 'D) er
(** [xE >>- xy] is equivalent to [map xy xE]. *)

val ( >-> ) :
  ('a -> ('R, 'e, 'b, (('R, 'D) #map' as 'D)) er) ->
  ('b -> 'c) ->
  'a ->
  ('R, 'e, 'c, 'D) er
(** [xyE >-> yz] is equivalent to [fun x -> map yz (xyE x)]. *)

val lift'1 :
  ('b -> 'a) -> ('R, 'e, 'b, (('R, 'D) #map' as 'D)) er -> ('R, 'e, 'a, 'D) er
(** [lift'1 xy xE] is equivalent to [map xy xE]. *)

(** {3 Pointed functors} *)

type ('R, 'e, 'a, 'D) pure'm = 'a -> ('R, 'e, 'a) s
(** {!pure} effect signature. *)

(** {!pure} capability mix-in. *)
class virtual ['R, 'D] pure' :
  object
    method virtual pure' : ('R, 'e, 'a, 'D) pure'm
    (** Implements the {!pure} capability. *)
  end

(** Pointed functor offers the {!map}, and {!pure} capabilities. *)
class virtual ['R, 'D] pointed' :
  object
    inherit ['R, 'D] map'
    inherit ['R, 'D] pure'
  end

val pure : 'a -> ('R, 'e, 'a, (('R, 'D) #pure' as 'D)) er
(** [pure value] effect. *)

val return : 'a -> ('R, 'e, 'a, (('R, 'D) #pure' as 'D)) er
(** [return value] is equivalent to [pure value]. *)

val unit : ('R, 'e, unit, (('R, 'D) #pure' as 'D)) er
(** [unit] is equivalent to [pure ()]. *)

val do_unless : bool -> ('R, 'e, unit, (('R, 'D) #pure' as 'D)) er op'1
(** [do_unless b uE] is equivalent to [if b then unit else uE]. *)

val do_when : bool -> ('R, 'e, unit, (('R, 'D) #pure' as 'D)) er op'1
(** [do_when b uE] is equivalent to [if b then uE else unit]. *)

(** {3 Applicative functors} *)

type ('R, 'e, 'a, 'b, 'D) pair'm =
  ('R, 'e, 'a, 'D) er -> ('R, 'e, 'b, 'D) er -> ('R, 'e, 'a * 'b) s
(** {!pair} effect signature. *)

(** {!pair} capability mix-in. *)
class virtual ['R, 'D] pair' :
  object
    method virtual pair' : ('R, 'e, 'a, 'b, 'D) pair'm
    (** Implements the {!pair} capability. *)
  end

(** Product functor offers the {!map}, and {!pair} capabilities. *)
class virtual ['R, 'D] product' :
  object
    inherit ['R, 'D] map'
    inherit ['R, 'D] pair'
  end

(** Applicative functor offers the {!map}, {!pure}, and {!pair}
    capabilities. *)
class virtual ['R, 'D] applicative' :
  object
    inherit ['R, 'D] pointed'
    inherit ['R, 'D] pair'
  end

val pair :
  ('R, 'e, 'a, (('R, 'D) #pair' as 'D)) er ->
  ('R, 'e, 'b, 'D) er ->
  ('R, 'e, 'a * 'b, 'D) er
(** [pair xE yE] effect. *)

val ( and+ ) :
  ('R, 'e, 'a, (('R, 'D) #pair' as 'D)) er ->
  ('R, 'e, 'b, 'D) er ->
  ('R, 'e, 'a * 'b, 'D) er
(** [( and+ ) xE yE] is equivalent to [pair xE yE]. *)

val ( <*> ) :
  ('R, 'e, 'a, (('R, 'D) #pair' as 'D)) er ->
  ('R, 'e, 'b, 'D) er ->
  ('R, 'e, 'a * 'b, 'D) er
(** [xE <*> yE] is equivalent to [pair xE yE]. *)

val tuple'2 :
  ('R, 'e, 'a, (('R, 'D) #pair' as 'D)) er ->
  ('R, 'e, 'b, 'D) er ->
  ('R, 'e, 'a * 'b, 'D) er
(** [tuple'2 x1E x2E] is equivalent to [pair x1E x2E]. *)

val tuple'3 :
  ('R, 'e, 'a1, (('R, 'D) #product' as 'D)) er ->
  ('R, 'e, 'a2, 'D) er ->
  ('R, 'e, 'a3, 'D) er ->
  ('R, 'e, 'a1 * 'a2 * 'a3, 'D) er
(** [tuple'3 x1E x2E x3E] is equivalent to
    [map (fun (x1, (x2, x3)) -> (x1, x2, x3)) (pair x1E (pair x2E x3E))]. *)

val tuple'4 :
  ('R, 'e, 'a1, (('R, 'D) #product' as 'D)) er ->
  ('R, 'e, 'a2, 'D) er ->
  ('R, 'e, 'a3, 'D) er ->
  ('R, 'e, 'a4, 'D) er ->
  ('R, 'e, 'a1 * 'a2 * 'a3 * 'a4, 'D) er
(** [tuple'4 x1E x2E x3E x4E] is like {!tuple'3}, but for 4 elements. *)

val tuple'5 :
  ('R, 'e, 'a1, (('R, 'D) #product' as 'D)) er ->
  ('R, 'e, 'a2, 'D) er ->
  ('R, 'e, 'a3, 'D) er ->
  ('R, 'e, 'a4, 'D) er ->
  ('R, 'e, 'a5, 'D) er ->
  ('R, 'e, 'a1 * 'a2 * 'a3 * 'a4 * 'a5, 'D) er
(** [tuple'5 x1E x2E x3E x4E x5E] is like {!tuple'3}, but for 5 elements. *)

val tuple'6 :
  ('R, 'e, 'a1, (('R, 'D) #product' as 'D)) er ->
  ('R, 'e, 'a2, 'D) er ->
  ('R, 'e, 'a3, 'D) er ->
  ('R, 'e, 'a4, 'D) er ->
  ('R, 'e, 'a5, 'D) er ->
  ('R, 'e, 'a6, 'D) er ->
  ('R, 'e, 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6, 'D) er
(** [tuple'6 x1E x2E x3E x4E x5E x6E] is like {!tuple'3}, but for 6 elements. *)

val map_er'1 : ('b -> 'd -> 'a) -> 'b -> 'd -> 'a
(** TODO *)

val map_er'2 :
  ('b1 -> ('R, 'e, 'a1, (('R, 'D) #product' as 'D)) er) ->
  ('b2 -> ('R, 'e, 'a2, 'D) er) ->
  'b1 * 'b2 ->
  ('R, 'e, 'a1 * 'a2, 'D) er
(** TODO *)

val map_er'3 :
  ('b1 -> ('R, 'e, 'a1, (('R, 'D) #product' as 'D)) er) ->
  ('b2 -> ('R, 'e, 'a2, 'D) er) ->
  ('b3 -> ('R, 'e, 'a3, 'D) er) ->
  'b1 * 'b2 * 'b3 ->
  ('R, 'e, 'a1 * 'a2 * 'a3, 'D) er
(** TODO *)

val map_er'4 :
  ('b1 -> ('R, 'e, 'a1, (('R, 'D) #product' as 'D)) er) ->
  ('b2 -> ('R, 'e, 'a2, 'D) er) ->
  ('b3 -> ('R, 'e, 'a3, 'D) er) ->
  ('b4 -> ('R, 'e, 'a4, 'D) er) ->
  'b1 * 'b2 * 'b3 * 'b4 ->
  ('R, 'e, 'a1 * 'a2 * 'a3 * 'a4, 'D) er
(** TODO *)

val map_er'5 :
  ('b1 -> ('R, 'e, 'a1, (('R, 'D) #product' as 'D)) er) ->
  ('b2 -> ('R, 'e, 'a2, 'D) er) ->
  ('b3 -> ('R, 'e, 'a3, 'D) er) ->
  ('b4 -> ('R, 'e, 'a4, 'D) er) ->
  ('b5 -> ('R, 'e, 'a5, 'D) er) ->
  'b1 * 'b2 * 'b3 * 'b4 * 'b5 ->
  ('R, 'e, 'a1 * 'a2 * 'a3 * 'a4 * 'a5, 'D) er
(** TODO *)

val map_er'6 :
  ('b1 -> ('R, 'e, 'a1, (('R, 'D) #product' as 'D)) er) ->
  ('b2 -> ('R, 'e, 'a2, 'D) er) ->
  ('b3 -> ('R, 'e, 'a3, 'D) er) ->
  ('b4 -> ('R, 'e, 'a4, 'D) er) ->
  ('b5 -> ('R, 'e, 'a5, 'D) er) ->
  ('b6 -> ('R, 'e, 'a6, 'D) er) ->
  'b1 * 'b2 * 'b3 * 'b4 * 'b5 * 'b6 ->
  ('R, 'e, 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6, 'D) er
(** TODO *)

val map_eq_er'1 : ('a -> 'd -> 'a) -> 'a -> 'd -> 'a
(** TODO *)

val map_eq_er'2 :
  ('a1 -> ('R, 'e, 'a1, (('R, 'D) #product' as 'D)) er) ->
  ('a2 -> ('R, 'e, 'a2, 'D) er) ->
  'a1 * 'a2 ->
  ('R, 'e, 'a1 * 'a2, 'D) er
(** TODO *)

val map_eq_er'3 :
  ('a1 -> ('R, 'e, 'a1, (('R, 'D) #product' as 'D)) er) ->
  ('a2 -> ('R, 'e, 'a2, 'D) er) ->
  ('a3 -> ('R, 'e, 'a3, 'D) er) ->
  'a1 * 'a2 * 'a3 ->
  ('R, 'e, 'a1 * 'a2 * 'a3, 'D) er
(** TODO *)

val map_eq_er'4 :
  ('a1 -> ('R, 'e, 'a1, (('R, 'D) #product' as 'D)) er) ->
  ('a2 -> ('R, 'e, 'a2, 'D) er) ->
  ('a3 -> ('R, 'e, 'a3, 'D) er) ->
  ('a4 -> ('R, 'e, 'a4, 'D) er) ->
  'a1 * 'a2 * 'a3 * 'a4 ->
  ('R, 'e, 'a1 * 'a2 * 'a3 * 'a4, 'D) er
(** TODO *)

val map_eq_er'5 :
  ('a1 -> ('R, 'e, 'a1, (('R, 'D) #product' as 'D)) er) ->
  ('a2 -> ('R, 'e, 'a2, 'D) er) ->
  ('a3 -> ('R, 'e, 'a3, 'D) er) ->
  ('a4 -> ('R, 'e, 'a4, 'D) er) ->
  ('a5 -> ('R, 'e, 'a5, 'D) er) ->
  'a1 * 'a2 * 'a3 * 'a4 * 'a5 ->
  ('R, 'e, 'a1 * 'a2 * 'a3 * 'a4 * 'a5, 'D) er
(** TODO *)

val map_eq_er'6 :
  ('a1 -> ('R, 'e, 'a1, (('R, 'D) #product' as 'D)) er) ->
  ('a2 -> ('R, 'e, 'a2, 'D) er) ->
  ('a3 -> ('R, 'e, 'a3, 'D) er) ->
  ('a4 -> ('R, 'e, 'a4, 'D) er) ->
  ('a5 -> ('R, 'e, 'a5, 'D) er) ->
  ('a6 -> ('R, 'e, 'a6, 'D) er) ->
  'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 ->
  ('R, 'e, 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6, 'D) er
(** TODO *)

val lift'2 :
  ('b1 -> 'b2 -> 'a) ->
  ('R, 'e, 'b1, (('R, 'D) #product' as 'D)) er ->
  ('R, 'e, 'b2, 'D) er ->
  ('R, 'e, 'a, 'D) er
(** [lift'2 xyz xE yE] is equivalent to
    [map (fun (x, y) -> xyz x y) (pair xE yE)]. *)

val lift'3 :
  ('b1 -> 'b2 -> 'b3 -> 'a) ->
  ('R, 'e, 'b1, (('R, 'D) #product' as 'D)) er ->
  ('R, 'e, 'b2, 'D) er ->
  ('R, 'e, 'b3, 'D) er ->
  ('R, 'e, 'a, 'D) er
(** TODO *)

val lift'4 :
  ('b1 -> 'b2 -> 'b3 -> 'b4 -> 'a) ->
  ('R, 'e, 'b1, (('R, 'D) #product' as 'D)) er ->
  ('R, 'e, 'b2, 'D) er ->
  ('R, 'e, 'b3, 'D) er ->
  ('R, 'e, 'b4, 'D) er ->
  ('R, 'e, 'a, 'D) er
(** TODO *)

val lift'5 :
  ('b1 -> 'b2 -> 'b3 -> 'b4 -> 'b5 -> 'a) ->
  ('R, 'e, 'b1, (('R, 'D) #product' as 'D)) er ->
  ('R, 'e, 'b2, 'D) er ->
  ('R, 'e, 'b3, 'D) er ->
  ('R, 'e, 'b4, 'D) er ->
  ('R, 'e, 'b5, 'D) er ->
  ('R, 'e, 'a, 'D) er
(** TODO *)

val lift'6 :
  ('b1 -> 'b2 -> 'b3 -> 'b4 -> 'b5 -> 'b6 -> 'a) ->
  ('R, 'e, 'b1, (('R, 'D) #product' as 'D)) er ->
  ('R, 'e, 'b2, 'D) er ->
  ('R, 'e, 'b3, 'D) er ->
  ('R, 'e, 'b4, 'D) er ->
  ('R, 'e, 'b5, 'D) er ->
  ('R, 'e, 'b6, 'D) er ->
  ('R, 'e, 'a, 'D) er
(** TODO *)

(** {3 Selective functors} *)

type ('R, 'e, 'a, 'b, 'c, 'D) branch'm =
  ('R, 'e, 'b -> 'a, 'D) er ->
  ('R, 'e, 'c -> 'a, 'D) er ->
  ('R, 'e, ('b, 'c) branch, 'D) er ->
  ('R, 'e, 'a) s
(** {!val-branch} effect signature. *)

(** {!val-branch} capability mix-in. *)
class virtual ['R, 'D] branch' :
  object
    method virtual branch' : ('R, 'e, 'a, 'b, 'c, 'D) branch'm
    (** Implements the {!val-branch} capability. *)
  end

(** Selective functor offers the {!map}, {!pure}, {!pair}, and {!val-branch}
    capabilities. *)
class virtual ['R, 'D] selective' :
  object
    inherit ['R, 'D] applicative'
    inherit ['R, 'D] branch'
  end

val branch :
  ('R, 'e, 'b -> 'a, 'D) er ->
  ('R, 'e, 'c -> 'a, 'D) er ->
  ('R, 'e, ('b, 'c) branch, (('R, 'D) #branch' as 'D)) er ->
  ('R, 'e, 'a, 'D) er
(** [branch baE caE bcE] effect. *)

val if_else_s :
  ('R, 'e, 'a, 'D) er ->
  ('R, 'e, 'a, 'D) er ->
  ('R, 'e, bool, (('R, 'D) #selective' as 'D)) er ->
  ('R, 'e, 'a, 'D) er
(** [if_else_s tE eE cE] is equivalent to
    {[
    branch
      (map const eE)
      (map const tE)
      (map (function true  -> Left  ()
                   | false -> Right ())
           cE) ]} *)

(** {3 Sequencing functors} *)

type ('R, 'e, 'a, 'b, 'D) bind'm =
  ('R, 'e, 'b, 'D) er -> ('b -> ('R, 'e, 'a, 'D) er) -> ('R, 'e, 'a) s
(** {!bind} effect signature. *)

(** {!bind} capability mix-in. *)
class virtual ['R, 'D] bind' :
  object
    method virtual bind' : ('R, 'e, 'a, 'b, 'D) bind'm
    (** Implements the {!bind} capability. *)
  end

(** Monad offers the {!map}, {!pure}, {!pair}, {!val-branch}, and {!bind}
    capabilities. *)
class virtual ['R, 'D] monad' :
  object
    inherit ['R, 'D] selective'
    inherit ['R, 'D] bind'
  end

(** Implements defaults for {!map}, {!pair}, and {!val-branch} in terms of
    {!pure} and {!bind}. *)
class virtual ['R, 'D] monad'd :
  object ('D)
    inherit ['R, 'D] monad'
    method map' : ('R, 'e, 'a, 'b, 'D) map'm
    method pair' : ('R, 'e, 'a, 'b, 'D) pair'm
    method branch' : ('R, 'e, 'a, 'b, 'c, 'D) branch'm
  end

val bind :
  ('R, 'e, 'b, (('R, 'D) #bind' as 'D)) er ->
  ('b -> ('R, 'e, 'a, 'D) er) ->
  ('R, 'e, 'a, 'D) er
(** [bind xE xyE] effect. *)

val ( >>= ) :
  ('R, 'e, 'b, (('R, 'D) #bind' as 'D)) er ->
  ('b -> ('R, 'e, 'a, 'D) er) ->
  ('R, 'e, 'a, 'D) er
(** [xE >>= xyE] is equivalent to [bind xE xyE]. *)

val ( let* ) :
  ('R, 'e, 'b, (('R, 'D) #bind' as 'D)) er ->
  ('b -> ('R, 'e, 'a, 'D) er) ->
  ('R, 'e, 'a, 'D) er
(** [( let* ) xE xyE] is equivalent to [bind xyE xE]. *)

val ( and* ) :
  ('R, 'e, 'a, (('R, 'D) #pair' as 'D)) er ->
  ('R, 'e, 'b, 'D) er ->
  ('R, 'e, 'a * 'b, 'D) er
(** [( and* ) xE yE] is equivalent to [pair xE yE]. *)

val join :
  ('R, 'e, ('R, 'e, 'a, (('R, 'D) #bind' as 'D)) er, 'D) er ->
  ('R, 'e, 'a, 'D) er
(** [join xEE] is equivalent to [bind xEE (fun xE -> xE)]. *)

val ( >> ) :
  ('R, 'e, unit, (('R, 'D) #bind' as 'D)) er -> ('R, 'e, 'a, 'D) er op'1
(** [uE >> xE] is equivalent to [bind uE (fun () -> xE)]. *)

val ( >=> ) :
  ('a -> ('R, 'e, 'b, (('R, 'D) #bind' as 'D)) er) ->
  ('b -> ('R, 'e, 'c, 'D) er) ->
  'a ->
  ('R, 'e, 'c, 'D) er
(** [xyE >=> yzE] is equivalent to [fun x -> bind (xyE x) yzE]. *)

val ( ||| ) : ('R, 'e, bool, (('R, 'D) #monad' as 'D)) er op'2
(** [lE &&& rE] is equivalent to
    [bind lE (function true -> pure true | false -> rE)]. *)

val ( &&& ) : ('R, 'e, bool, (('R, 'D) #monad' as 'D)) er op'2
(** [lE &&& rE] is equivalent to
    [bind lE (function true -> rE | false -> pure true)]. *)

(** {3 Alternatives} *)

type ('R, 'e, 'a, 'D) zero'm = ('R, 'e, 'a) s
(** {!zero} effect signature. *)

(** {!zero} capability mix-in. *)
class virtual ['R, 'D] zero' :
  object
    method virtual zero' : ('R, 'e, 'a, 'D) zero'm
    (** Implements the {!zero} capability. *)
  end

type ('R, 'e, 'a, 'D) alt'm =
  ('R, 'e, 'a, 'D) er -> ('R, 'e, 'a, 'D) er -> ('R, 'e, 'a) s
(** {!alt} effect signature. *)

(** {!alt} capability mix-in. *)
class virtual ['R, 'D] alt' :
  object
    method virtual alt' : ('R, 'e, 'a, 'D) alt'm
    (** Implements the {!alt} capability. *)
  end

(** Plus offers the {!zero} and {!alt} capabilities. *)
class virtual ['R, 'D] plus' :
  object
    inherit ['R, 'D] zero'
    inherit ['R, 'D] alt'
  end

val zero : ('R, 'e, 'a, (('R, 'D) #zero' as 'D)) er
(** [zero] effect. *)

val alt : ('R, 'e, 'a, (('R, 'D) #alt' as 'D)) er op'2
(** [alt lE rE] effect. *)

val ( <|> ) : ('R, 'e, 'a, (('R, 'D) #alt' as 'D)) er op'2
(** [lE <|> rE] is equivalent to [alt lE rE]. *)

val iota :
  int -> ('R, 'e, int, (< ('R, 'D) pure' ; ('R, 'D) plus' ; .. > as 'D)) er
(** TODO *)

val filter :
  ('a -> bool) ->
  ('R, 'e, 'a, (< ('R, 'D) monad' ; ('R, 'D) zero' ; .. > as 'D)) er ->
  ('R, 'e, 'a, 'D) er
(** TODO *)

(** {3 Error handling} *)

type ('R, 'e, 'a, 'D) fail'm = 'e -> ('R, 'e, 'a) s
(** {!fail} effect signature. *)

(** {!fail} capability mix-in. *)
class virtual ['R, 'D] fail' :
  object
    method virtual fail' : ('R, 'e, 'a, 'D) fail'm
    (** Implements the {!fail} capability. *)
  end

type ('R, 'e, 'f, 'a, 'b, 'D) tryin'm =
  ('f -> ('R, 'e, 'a, 'D) er) ->
  ('b -> ('R, 'e, 'a, 'D) er) ->
  ('R, 'f, 'b, 'D) er ->
  ('R, 'e, 'a) s
(** {!tryin} effect signature. *)

(** {!tryin} capability mix-in. *)
class virtual ['R, 'D] tryin' :
  object
    method virtual tryin' : ('R, 'e, 'f, 'a, 'b, 'D) tryin'm
    (** Implements the {!tryin} capability. *)
  end

(** Error handling offers the {!fail}, and {!tryin}. *)
class virtual ['R, 'D] errors' :
  object
    inherit ['R, 'D] fail'
    inherit ['R, 'D] tryin'
  end

val fail : 'e -> ('R, 'e, 'a, (('R, 'D) #fail' as 'D)) er
(** [fail error] effect. *)

val tryin :
  ('f -> ('R, 'e, 'a, (('R, 'D) #tryin' as 'D)) er) ->
  ('b -> ('R, 'e, 'a, 'D) er) ->
  ('R, 'f, 'b, 'D) er ->
  ('R, 'e, 'a, 'D) er
(** [tryin exE yxE xE] effect. *)

val catch :
  ('R, 'e, 'a, (< ('R, 'D) pure' ; ('R, 'D) tryin' ; .. > as 'D)) er ->
  ('R, 'f, ('e, 'a) res, 'D) er
(** [catch xE] is equivalent to
    [tryIn (fun e -> pure (`Error e)) (fun x -> pure (`Ok x)) xE]. *)

val handle :
  ('f -> ('R, 'e, 'a, (< ('R, 'D) pure' ; ('R, 'D) tryin' ; .. > as 'D)) er) ->
  ('R, 'f, 'a, 'D) er ->
  ('R, 'e, 'a, 'D) er
(** [handle exE xE] is equivalent to [tryin exE pure xE] *)

val finally :
  ('R, 'e, unit, (< ('R, 'D) monad' ; ('R, 'D) errors' ; .. > as 'D)) er ->
  ('R, 'e, 'a, 'D) er op'1
(** [finally uE xE] is equivalent to
    [tryin (fun e -> bind uE (fun () -> fail e)) (fun x -> bind uE (fun () -> pure x)) xE]. *)

val map_error :
  ('f -> 'e) ->
  ('R, 'f, 'a, (< ('R, 'D) pure' ; ('R, 'D) errors' ; .. > as 'D)) er ->
  ('R, 'e, 'a, 'D) er
(** TODO *)

val gen_error :
  ('R, nothing, 'a, (< ('R, 'D) pure' ; ('R, 'D) errors' ; .. > as 'D)) er ->
  ('R, 'e, 'a, 'D) er
(** TODO *)

(** {3 Asynchronicity} *)

type ('R, 'e, 'a, 'b, 'D) par'm = ('R, 'e, 'a, 'b, 'D) pair'm
(** {!par} effect signature. *)

(** {!par} effect signature. *)
class virtual ['R, 'D] par' :
  object
    method virtual par' : 'e 'a 'b. ('R, 'e, 'a, 'b, 'D) par'm
  end

val par :
  ('R, 'e, 'a, (('R, 'D) #par' as 'D)) er ->
  ('R, 'e, 'b, 'D) er ->
  ('R, 'e, 'a * 'b, 'D) er
(** [par xE yE] effect. *)

type ('R, 'e, 'a, 'D) suspend'm = (('e, 'a) res, unit) cps -> ('R, 'e, 'a) s
(** {!suspend} effect signature. *)

(** {!suspend} capability mix-in. *)
class virtual ['R, 'D] suspend' :
  object
    method virtual suspend' : ('R, 'e, 'a, 'D) suspend'm
    (** Implements the {!suspend} capability. *)
  end

val suspend :
  (('e, 'a) res, unit) cps -> ('R, 'e, 'a, (('R, 'D) #suspend' as 'D)) er
(** [suspend with_resume] effect. *)

type ('R, 'e, 'D) spawn'm = ('R, nothing, unit, 'D) er -> ('R, 'e, unit) s
(** {!spawn} effect signature. *)

(** {!spawn} effect capability. *)
class virtual ['R, 'D] spawn' :
  object
    method virtual spawn' : 'e. ('R, 'e, 'D) spawn'm
  end

(** TODO *)
class virtual ['R, 'D] async' :
  object
    inherit ['R, 'D] monad'
    inherit ['R, 'D] errors'
    inherit ['R, 'D] suspend'
    inherit ['R, 'D] par'
    inherit ['R, 'D] spawn'
  end

val spawn :
  ('R, nothing, unit, (('R, 'D) #spawn' as 'D)) er -> ('R, 'e, unit, 'D) er
(** [spawn uE] effect. *)

(** Implements {!par}. *)
class virtual ['R, 'D] par'd :
  object ('D)
    inherit ['R, 'D] monad'
    inherit ['R, 'D] tryin'
    inherit ['R, 'D] suspend'
    inherit ['R, 'D] spawn'
    inherit ['R, 'D] par'
    method par' : ('R, 'e, 'a, 'b, 'D) par'm
  end

(** Memoized lazy computation for asynchronous programming. *)
module Memo : sig
  type ('R, 'e, 'a) t
  (** Represents a memoized lazy computation. *)

  val create :
    ( 'R,
      'f,
      'a,
      (< ('R, 'D) monad' ; ('R, 'D) errors' ; ('R, 'D) suspend' ; .. > as 'D)
    )
    er ->
    ('R, 'e, ('R, 'f, 'a) t, 'D) er
  (** [create eaE] effect returns a memoized lazy computation. *)

  val eval :
    ('R, 'e, 'a) t ->
    ( 'R,
      'e,
      'a,
      (< ('R, 'D) monad' ; ('R, 'D) errors' ; ('R, 'D) suspend' ; .. > as 'D)
    )
    er
  (** [eval eaM] effect executes the effect and memoizes its result or produces
      the memoized result. *)
end

(** Mutable ref cells for asynchronous programming.

    Operations on mutable ref cells are atomic such that when an update of a ref
    cell is in progress other operations on the ref cell will suspend. *)
module Mut : sig
  type 'v t
  (** Represents a mutable ref cell for asynchronous programming. *)

  val create : 'v -> 'v t
  (** [create value] creates a new mutable ref cell with given [value]. *)

  val read :
    'v t ->
    ('R, 'e, 'v, (< ('R, 'D) pointed' ; ('R, 'D) suspend' ; .. > as 'D)) er
  (** [read mut] effect returns the current value of the mutable ref cell. *)

  val mutate :
    'v op'1 ->
    'v t ->
    ('R, 'e, unit, (< ('R, 'D) pointed' ; ('R, 'D) suspend' ; .. > as 'D)) er
  (** [mutate fn mut] effect updates the mutable ref cell with the given
      synchronous function. *)

  val modify :
    ('v -> 'v * 'a) ->
    'v t ->
    ('R, 'e, 'a, (< ('R, 'D) pointed' ; ('R, 'D) suspend' ; .. > as 'D)) er
  (** [modify fn mut] effect updates the mutable ref cell with the given
      synchronous function. *)

  val try_mutate :
    ('v -> ('R, 'e, 'v, 'D) er) ->
    'v t ->
    ( 'R,
      'e,
      unit,
      (< ('R, 'D) monad' ; ('R, 'D) errors' ; ('R, 'D) suspend' ; .. > as 'D)
    )
    er
  (** [try_mutate ef mut] effect updates the mutable ref cell with the given
      asynchronous effect. *)

  val try_modify :
    ('v -> ('R, 'e, 'v * 'a, 'D) er) ->
    'v t ->
    ( 'R,
      'e,
      'a,
      (< ('R, 'D) monad' ; ('R, 'D) errors' ; ('R, 'D) suspend' ; .. > as 'D)
    )
    er
  (** [try_modify ef mut] effect updates the mutable ref cell with the given
      asynchronous effect. *)
end

(** {3 Environment}

    The environment or reader is built-in to the effect system in the form of
    the capability dictionary.  Thanks to OCaml's structural objects, it is
    possible to extend the dictionary for user needs. *)

(** {4 Dictionary} *)

val env : ('R, 'e, (('R, 'D) #pure' as 'D), 'D) er
(** [env] effect returns the dictionary ['d] of capabilities. *)

val env_as : ((('R, 'D) #pure' as 'D) -> 'a) -> ('R, 'e, 'a, 'D) er
(** [env_as fn] effect returns the value of type ['a] computed from the
    dictionary of capabilities of type ['d]. *)

val mapping_env : ('D -> 'S) -> ('R, 'e, 'a, 'S) er -> ('R, 'e, 'a, 'D) er
(** [mapping_env fn er] effect executes the effect [er] with the dictionary of
    capabilities of type ['D] mapped through the given function [fn]. *)

val setting_env : 's -> ('R, 'e, 'a, 's) er -> ('R, 'e, 'a, 'D) er
(** [setting_env s er] effect executes the effect [er] with the given dictionary
    [s] of capabilities. *)

(** {4 Properties} *)

(** An abstraction for accessing instance variables or properties of objects. *)
module Prop : sig
  type 'a t
  (** Represents an updatable instance variable or property of an object. *)

  val make : (unit -> 'a) -> ('a -> unit) -> 'a t
  (** [make get set] specifies a property from [get] and [set]. *)

  val get : ((< .. > as 'o) -> 'a t) -> 'o -> 'a
  (** [get prop_of obj] gets the property. *)

  val set : ((< .. > as 'o) -> 'a t) -> 'a -> 'o -> 'o
  (** [set prop_of v obj] functionally updates the property. *)

  val map : ((< .. > as 'o) -> 'a t) -> ('a -> 'a) -> 'o -> 'o
  (** [map prop_of fn obj] functionally updates the property. *)
end

val prop : (unit -> 'a) -> ('a -> unit) -> 'a Prop.t
(** [prop get set] is equivalent to [Prop.make get set]. *)

val get : ((('R, 'D) #pure' as 'D) -> 'a Prop.t) -> ('R, 'e, 'a, 'D) er
(** [get prop_of] effect returns the value of the property from the
    dictionary of capabilities of type ['d]. *)

val get_as :
  ((('R, 'D) #pure' as 'D) -> 'b Prop.t) -> ('b -> 'a) -> ('R, 'e, 'a, 'D) er
(** [get_as prop_of fn] *)

val mapping :
  ((< .. > as 'D) -> 'p Prop.t) -> 'p op'1 -> ('R, 'e, 'a, 'D) er op'1
(** [mapping prop_of fn xE] effect executes the effect [xE] with the property of
    the dictionary of capabilities mapped through the given function. *)

val setting : ((< .. > as 'D) -> 'p Prop.t) -> 'p -> ('R, 'e, 'a, 'D) er op'1
(** [setting prop_of value xE] effect executess the effect [xE] with the
    property of the dictionary of capabilities set to given [value]. *)

(** {5 Mutable properties} *)

val read :
  ('D -> 'v Mut.t Prop.t) ->
  ('R, 'e, 'v, (< ('R, 'D) monad' ; ('R, 'D) suspend' ; .. > as 'D)) er
(** [read prop_of] effect. *)

val mutate :
  ('D -> 'v Mut.t Prop.t) ->
  'v op'1 ->
  ('R, 'e, unit, (< ('R, 'D) monad' ; ('R, 'D) suspend' ; .. > as 'D)) er
(** [mutate prop_of vv] effect. *)

val modify :
  ('D -> 'v Mut.t Prop.t) ->
  ('v -> 'v * 'a) ->
  ('R, 'e, 'a, (< ('R, 'D) monad' ; ('R, 'D) suspend' ; .. > as 'D)) er
(** [modify prop_of vva] effect. *)

val try_mutate :
  ('D -> 'v Mut.t Prop.t) ->
  ('v -> ('R, 'e, 'v, 'D) er) ->
  ( 'R,
    'e,
    unit,
    (< ('R, 'D) monad' ; ('R, 'D) errors' ; ('R, 'D) suspend' ; .. > as 'D) )
  er
(** [try_mutate prop_of vvE] effect. *)

val try_modify :
  ('D -> 'v Mut.t Prop.t) ->
  ('v -> ('R, 'e, 'v * 'a, 'D) er) ->
  ( 'R,
    'e,
    'a,
    (< ('R, 'D) monad' ; ('R, 'D) errors' ; ('R, 'D) suspend' ; .. > as 'D) )
  er
(** [try_modify prop_of vvaE] effect. *)

val cloning :
  ('D -> 'v Mut.t Prop.t) ->
  ('R, 'e, 'a, (< ('R, 'D) monad' ; ('R, 'D) suspend' ; .. > as 'D)) er op'1
(** [cloning prop_of xE] effect. *)

(** {2 Interpreters}

    Users can and often should implement their own interpreters that only allow
    specific limited sets of effects.  This package is intended to only provide
    the core framework for effectul programming. *)

(** {3 for Stdlib} *)

(** TODO *)
module StdRea : sig
  (** TODO *)
  module List : sig
    val map_er :
      ('a -> ('R, 'e, 'b, (('R, 'D) #applicative' as 'D)) er) ->
      'a List.t ->
      ('R, 'e, 'b List.t, 'D) er
    (** TODO *)

    val map_eq_er :
      ('a -> ('R, 'e, 'a, (('R, 'D) #applicative' as 'D)) er) ->
      'a List.t ->
      ('R, 'e, 'a List.t, 'D) er
    (** TODO *)

    type r
    (** TODO *)

    val to_rea : 'a List.t -> (r, 'e, 'a) s
    (** TODO *)

    val of_rea : (r, 'e, 'a) s -> 'a List.t
    (** TODO *)

    (** TODO *)
    class ['D] monad_plus :
      object ('D)
        method map' : (r, 'e, 'a, 'b, 'D) map'm
        method pure' : (r, 'e, 'a, 'D) pure'm
        method pair' : (r, 'e, 'a, 'b, 'D) pair'm
        method branch' : (r, 'e, 'a, 'b, 'c, 'D) branch'm
        method bind' : (r, 'e, 'a, 'b, 'D) bind'm
        method zero' : (r, 'e, 'a, 'D) zero'm
        method alt' : (r, 'e, 'a, 'D) alt'm
      end

    val monad_plus : 'D monad_plus as 'D
    (** TODO *)
  end

  (** TODO *)
  module Option : sig
    val map_er :
      ('a -> ('R, 'e, 'b, (('R, 'D) #pointed' as 'D)) er) ->
      'a Option.t ->
      ('R, 'e, 'b Option.t, 'D) er
    (** TODO *)

    val map_eq_er :
      ('a -> ('R, 'e, 'a, (('R, 'D) #pointed' as 'D)) er) ->
      'a Option.t ->
      ('R, 'e, 'a Option.t, 'D) er
    (** TODO *)

    type r
    (** TODO *)

    val to_rea : 'a Option.t -> (r, 'e, 'a) s
    (** TODO *)

    val of_rea : (r, 'e, 'a) s -> 'a Option.t
    (** TODO *)

    (** TODO *)
    class ['D] monad_plus :
      object ('D)
        method map' : (r, 'e, 'a, 'b, 'D) map'm
        method pure' : (r, 'e, 'a, 'D) pure'm
        method pair' : (r, 'e, 'a, 'b, 'D) pair'm
        method branch' : (r, 'e, 'a, 'b, 'c, 'D) branch'm
        method bind' : (r, 'e, 'a, 'b, 'D) bind'm
        method zero' : (r, 'e, 'a, 'D) zero'm
        method alt' : (r, 'e, 'a, 'D) alt'm
      end

    val monad_plus : 'D monad_plus as 'D
    (** TODO *)
  end

  (** TODO *)
  module Result : sig
    type r
    (** TODO *)

    val to_rea : ('a, 'e) Result.t -> (r, 'e, 'a) s
    (** TODO *)

    val of_rea : (r, 'e, 'a) s -> ('a, 'e) Result.t
    (** TODO *)

    (** TODO *)
    class ['D] monad_errors :
      object ('D)
        method map' : (r, 'e, 'a, 'b, 'D) map'm
        method pure' : (r, 'e, 'a, 'D) pure'm
        method pair' : (r, 'e, 'a, 'b, 'D) pair'm
        method branch' : (r, 'e, 'a, 'b, 'c, 'D) branch'm
        method bind' : (r, 'e, 'a, 'b, 'D) bind'm
        method fail' : (r, 'e, 'a, 'D) fail'm
        method tryin' : (r, 'e, 'f, 'a, 'b, 'D) tryin'm
      end

    val monad_errors : 'D monad_errors as 'D
    (** TODO *)
  end

  (** TODO *)
  module Seq : sig
    val map_er :
      ('a -> ('R, 'e, 'b, (('R, 'D) #applicative' as 'D)) er) ->
      'a Seq.t ->
      ('R, 'e, 'b Seq.t, 'D) er
    (** TODO *)

    type r
    (** TODO *)

    val to_rea : 'a Seq.t -> (r, 'e, 'a) s
    (** TODO *)

    val of_rea : (r, 'e, 'a) s -> 'a Seq.t
    (** TODO *)

    (** TODO *)
    class ['D] monad_plus :
      object ('D)
        method map' : (r, 'e, 'a, 'b, 'D) map'm
        method pure' : (r, 'e, 'a, 'D) pure'm
        method pair' : (r, 'e, 'a, 'b, 'D) pair'm
        method branch' : (r, 'e, 'a, 'b, 'c, 'D) branch'm
        method bind' : (r, 'e, 'a, 'b, 'D) bind'm
        method zero' : (r, 'e, 'a, 'D) zero'm
        method alt' : (r, 'e, 'a, 'D) alt'm
      end

    val monad_plus : 'D monad_plus as 'D
    (** TODO *)
  end
end

(** {3 Data types} *)

(** Constant functor, products, and applicatives. *)
module Constant : sig
  type 'c r
  (** Abstract higher-kinded effect representation type constructor. *)

  val to_rea : 'c -> ('c r, 'e, 'a) s
  (** TODO *)

  val of_rea : ('c r, 'e, 'a) s -> 'c
  (** TODO *)

  val from : 'c -> ('c r, 'e, 'a, 'D) er
  (** [from value] has no effect aside from encapsulating the given [value]. *)

  (** {4 Constant functr} *)

  (** Base constant functor interpreter dictionary. *)
  class ['c, 'D] functr :
    object ('D)
      method map' : ('c r, 'e, 'a, 'b, 'D) map'm
    end

  val functr : ('c, 'D) functr as 'D
  (** Default {!class-functr} dictionary. *)

  (** {4 Constant applicatives} *)

  val conjunction : (bool r, 'D) applicative' as 'D
  (** Combines booleans with [&&] lazily. *)

  val disjunction : (bool r, 'D) applicative' as 'D
  (** Combines booleans with [||] lazily. *)

  val option : ('c option r, 'D) applicative' as 'D
  (** Combines options to return the leftmost some lazily. *)

  val unit_er :
    (('R, 'e, unit, (('R, 'D) #monad' as 'D)) er r, 'S) applicative' as 'S
  (** Combines unit effects with [bind]. *)

  val conjunction_er :
    (('R, 'e, bool, (('R, 'D) #monad' as 'D)) er r, 'S) applicative' as 'S
  (** Combines boolean effects with [&&&] lazily. *)

  val disjunction_er :
    (('R, 'e, bool, (('R, 'D) #monad' as 'D)) er r, 'S) applicative' as 'S
  (** Combines boolean effects with [|||] lazily. *)

  val option_er :
    (('R, 'e, 'c option, (('R, 'D) #monad' as 'D)) er r, 'S) applicative' as 'S
  (** Combines option effects to return the leftmost some lazily. *)

  (** {4 User defined} *)

  val map : 'D -> ('c r, 'e, 'a, 'b, 'D) map'm
  (** Use to implement the {!map} capability for a constant functr. *)

  val pure_of : 'c -> ('c r, 'e, 'a, 'D) pure'm
  (** Use to implement the {!pure} capability for a constant with the given
      [identity] element. *)

  val pair_with : 'D -> 'c lazy_op'2 -> ('c r, 'e, 'a, 'b, 'D) pair'm
  (** Use to implement the {!pair} capability for a constant with the given
      binary [combine] operator. *)

  (** [new product combine] creates the dictionary for a constant product
      functr. *)
  class ['c, 'D] product :
    'c lazy_op'2
    -> object ('D)
         method map' : ('c r, 'e, 'a, 'b, 'D) map'm
         method pair' : ('c r, 'e, 'a, 'b, 'D) pair'm
       end

  (** [new applicative identity combine] creates the dictionary for a constant
      applicative functr. *)
  class ['c, 'D] applicative :
    'c
    -> 'c lazy_op'2
    -> object ('D)
         method map' : ('c r, 'e, 'a, 'b, 'D) map'm
         method pure' : ('c r, 'e, 'a, 'D) pure'm
         method pair' : ('c r, 'e, 'a, 'b, 'D) pair'm
       end
end

(** Identity monad. *)
module Identity : sig
  type r
  (** Abstract higher-kinded effect representation type constructor. *)

  val to_rea : 'a -> (r, 'e, 'a) s
  (** TODO *)

  val of_rea : (r, 'e, 'a) s -> 'a
  (** TODO *)

  (** Base identity monad dictionary. *)
  class ['D] monad :
    object ('D)
      method map' : (r, 'e, 'a, 'b, 'D) map'm
      method pure' : (r, 'e, 'a, 'D) pure'm
      method pair' : (r, 'e, 'a, 'b, 'D) pair'm
      method branch' : (r, 'e, 'a, 'b, 'c, 'D) branch'm
      method bind' : (r, 'e, 'a, 'b, 'D) bind'm
    end

  val monad : (r, 'D) monad' as 'D
  (** Default {!class-monad} dictionary. *)
end

(** {3 Tail recursive} *)

(** A self tail recursive interpreter usable with {{:
    https://ocsigen.org/js_of_ocaml/latest/manual/tailcall} Js_of_ocaml}. *)
module Tailrec : sig
  type r
  (** Abstract higher-kinded effect representation type constructor. *)

  (** Base synchronous interpreter dictionary. *)
  class ['D] sync :
    object ('D)
      method map' : (r, 'e, 'a, 'b, 'D) map'm
      method pure' : (r, 'e, 'a, 'D) pure'm
      method pair' : (r, 'e, 'a, 'b, 'D) pair'm
      method branch' : (r, 'e, 'a, 'b, 'c, 'D) branch'm
      method bind' : (r, 'e, 'a, 'b, 'D) bind'm
      method fail' : (r, 'e, 'a, 'D) fail'm
      method tryin' : (r, 'e, 'f, 'a, 'b, 'D) tryin'm

      method suspend' : 'D
      (** Incompatible with {!class-async} for safety. *)
    end

  val sync : 'D sync as 'D
  (** Default {!class-sync} dictionary. *)

  val run : ('D #sync as 'D) -> (r, 'e, 'a, 'D) er -> ('e, 'a) res
  (** [run d sync] executes the [sync] effect with the given interpreter [d] and
      returns its result. *)

  (** Base asynchronous interpreter dictionary. *)
  class ['D] async :
    object ('D)
      method map' : (r, 'e, 'a, 'b, 'D) map'm
      method pure' : (r, 'e, 'a, 'D) pure'm
      method pair' : (r, 'e, 'a, 'b, 'D) pair'm
      method branch' : (r, 'e, 'a, 'b, 'c, 'D) branch'm
      method bind' : (r, 'e, 'a, 'b, 'D) bind'm
      method fail' : (r, 'e, 'a, 'D) fail'm
      method tryin' : (r, 'e, 'f, 'a, 'b, 'D) tryin'm
      method par' : (r, 'e, 'a, 'b, 'D) par'm
      method suspend' : (r, 'e, 'a, 'D) suspend'm
      method spawn' : (r, 'e, 'D) spawn'm
    end

  val async : 'D async as 'D
  (** Default {!class-async} dictionary. *)

  val spawn : 'D -> (r, nothing, unit, 'D) er -> unit
  (** [spawn d async] queues the [async] effect for execution using the given
      interpreter [d]. *)
end

(** {2 Traversals} *)

(** TODO *)
module Traverse : sig
  val to_map :
    (('b -> ((Identity.r as 'R), 'e, 'a, 'D) er) ->
    't ->
    ('R, 'e, 's, (('R, 'D) monad' as 'D)) er) ->
    ('b -> 'a) ->
    't ->
    's
  (** TODO *)

  val to_set :
    (('b -> ((Identity.r as 'R), 'e, 'a, 'D) er) ->
    't ->
    ('R, 'e, 's, (('R, 'D) monad' as 'D)) er) ->
    'a ->
    't ->
    's
  (** TODO *)

  val to_map_constant :
    (('b -> (('c Constant.r as 'R), 'e, 'a, 'D) er) ->
    't ->
    ('R, 'e, 's, 'D) er) ->
    'D ->
    ('b -> 'c) ->
    't ->
    'c

  val to_get :
    (('c -> (('c Constant.r as 'R), 'e, 'a, 'D) er) ->
    't ->
    ('R, 'e, 's, (('R, 'D) functr' as 'D)) er) ->
    't ->
    'c
  (** TODO *)

  val to_get_opt :
    (('c -> (('c option Constant.r as 'R), 'e, 'a, 'D) er) ->
    't ->
    ('R, 'e, 's, (('R, 'D) applicative' as 'D)) er) ->
    't ->
    'c option
  (** TODO *)

  val to_exists :
    (('b -> ((bool Constant.r as 'R), 'e, 'a, 'D) er) ->
    't ->
    ('R, 'e, 's, (('R, 'D) applicative' as 'D)) er) ->
    ('b -> bool) ->
    't ->
    bool
  (** TODO *)

  val to_find_map :
    (('b -> (('c option Constant.r as 'R), 'e, 'a, 'D) er) ->
    't ->
    ('R, 'e, 's, (('R, 'D) applicative' as 'D)) er) ->
    ('b -> 'c option) ->
    't ->
    'c option
  (** TODO *)

  val to_map_reduce :
    (('b -> (('c Constant.r as 'R), 'e, 'a, 'D) er) ->
    't ->
    ('R, 'e, 's, (('R, 'D) applicative' as 'D)) er) ->
    'c lazy_op'2 ->
    'c ->
    ('b -> 'c) ->
    't ->
    'c
  (** TODO *)

  val to_iter_er :
    (('b -> (('c Constant.r as 'R), 'e, 'a, 'D) er) ->
    't ->
    ('R, 'e, 's, (('R, 'D) applicative' as 'D)) er) ->
    ('b -> (('Ru, 'eu, unit, (('Ru, 'Du) #monad' as 'Du)) er as 'c)) ->
    't ->
    'c
  (** TODO *)

  val to_exists_er :
    (('b -> (('c Constant.r as 'R), 'e, 'a, 'D) er) ->
    't ->
    ('R, 'e, 's, (('R, 'D) applicative' as 'D)) er) ->
    ('b -> (('Ru, 'eu, bool, (('Ru, 'Du) #monad' as 'Du)) er as 'c)) ->
    't ->
    'c
  (** TODO *)

  val to_find_map_er :
    (('b -> (('c Constant.r as 'R), 'e, 'a, 'D) er) ->
    't ->
    ('R, 'e, 's, (('R, 'D) applicative' as 'D)) er) ->
    ('b -> (('Ru, 'eu, 'x option, (('Ru, 'Du) #monad' as 'Du)) er as 'c)) ->
    't ->
    'c
  (** TODO *)
end
