# Effectful OCaml with Objects and Variants

This is a framework for generic composable effectful asynchronous programming
basically using only objects and polymorphic variants.

Features:

- Ability to write
  ["monad generic code"](https://discuss.ocaml.org/t/abstracting-over-monads-lwt-and-async-and/965).
- Functors, Applicatives, Monads, ...
- Extensible environment or reader.
- Laziness via η-expansion.
- Inferred checked exception handing.
- Asynchronous computations.
- Extensible with user defined effects.
- Plays well with type inference allowing relatively concise code.
- Interoperable with existing monadic libraries.

More specifically, this uses a
[tagless-final approach](https://okmij.org/ftp/tagless-final/course/optimizations.html#primer)
with the signatures specified using object types and a
[higher-kinded type encoding](https://github.com/ocamllabs/higher). Programs can
be written against abstract generic interfaces allowing the same program to be
executed with multiple interpreters. Interpreters are implemented as objects and
[passed via the reader pattern](https://www.haskellforall.com/2012/05/scrap-your-type-classes.html?showComment=1338305803531#c5117471807774445845).
Polymorphic variants can be used for errors. The end result is a system that
provides a variety of effects (environment, checked exceptions, asynchronicity,
...) _à la carte_.

This library requires OCaml version 4.08 or later for
[binding operators](https://v2.ocaml.org/manual/bindingops.html) and some other
convenience features, but it seems a library like this could have already been
written for OCaml 3 (released in 2000). All the elements of this approach have
been known at least since 2014.

## Introduction

The following subsections introduce some aspects of the Rea framework via simple
examples. The code snippets are also extracted as a
[test](src/test/Rea/Introduction.ml) to ensure that they are accurate.

### Basics

To begin, we just `open` the `Rea` module, which brings a lot of generic
combinators to scope:

```ocaml
open Rea
```

Let's look at a rather familiar example, the implementation of a naïve
exponential time Fibonacci function as an effectful computation. We use such a
trivial example in order to focus on some of the basics of the Rea framework. It
is assumed that the reader has basic familiarity with monads and the like. If
not, rest assured, there is
[no shortage of material introducing monads](https://wiki.haskell.org/Monad_tutorials_timeline)
on the Internet.

```ocaml
module Fib = struct
```

And here is a naïvely written eager Fibonacci function implementation using the
`pure` and `lift'2` combinators from the framework:

```ocaml
  let rec eager n =
    if n <= 1 then
      pure n
    else
      lift'2 ( + ) (eager (n - 2)) (eager (n - 1))
```

The `pure` aka `return` combinator should already be familiar to you. The
`lift'2` combinator (sometimes called `map2`) combinator takes an ordinary
function of two plain value arguments and returns a function that works on
effectful computations. In this case, the `+` operator is used to combine the
results of the two recursive Fibonacci computations as a computation.

The below definition shows the type inferred for `eager` (after renaming some
type variables to match what is used in the Rea framework):

```ocaml
  let _ =
    (eager
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
```

The above undoubtedly looks rather complicated. We can simplify it by using type
abbreviations (class types) that are used in the definition of the framework
itself. In particular, each of the methods `map'`, `pair'`, and `pure'` seen
above are defined by classes of the same name and each of those classes takes
two type parameters `('R, 'D)`:

```ocaml
  let _ =
    (eager
      : int ->
        (< ('R, 'D) map' ; ('R, 'D) pair' ; ('R, 'D) pure' ; .. > as 'D) ->
        ('R, 'e, int) s)
```

The curious recursive use of the `'D` type parameter makes sure that all of the
individual elements of the combined type agree on the type of the whole
composition.

Why do the class names have an apostrophe at the end? IOW, why `map'` instead of
just `map`? Well, there are lots of such a classes, multiple are often used in
type signatures, and they tend to have fairly common names. The apostrophe is
there to allow the classes to be conveniently at the top level of the `Rea`
module with reduced risk of naming collisions with other libraries.

Recall that we used two combinators `pure` and `lift'2` from the framework in
the `eager` function. Obviously `pure` corresponds to or requires the `pure'`
class. `lift'2` requires both of the `map'` and `pair'` classes. OCaml
conveniently infers the required combination for us.

We can simplify the type further. The combination of `map'`, `pair'`, and
`pure'` is well known. That combination is the signature of the so called
applicative functor. The Rea library defines the abbreviation `applicative'` (as
a class) for the combination:

```ocaml
  let _ = (eager : int -> (('R, 'D) #applicative' as 'D) -> ('R, 'e, int) s)
```

We are not quite done yet. Ignore the `int ->`. The remaining part of the type
is of the form `'D -> ('R, 'e, 'a) s`. This is the type of "effect readers" of
the `('R, 'e, 'a, 'D) er` type which is actually the main abstraction of the Rea
framework. Combinators in the Rea framework take effect readers as arguments and
return effect readers as results. So, we end up with the following equivalent
type:

```ocaml
  let _ = (eager : int -> ('R, 'e, int, (('R, 'D) #applicative' as 'D)) er)
```

Let's discuss the general form of the effect reader type `('R, 'e, 'a, 'D) er`.
The type has four type arguments:

- `'R` represents a higher-kinded abstract type constructor that is specific to
  a particular representation of effects.

- `'e` is the type of errors or failures that may be signaled during
  interpretation of the effect reader.

- `'a` is the type of answers or results of the effect reader in case it does
  not signal an error.

- `'D` is the type of the dictionary of capabilities or of the environment
  required by the effect reader. In other words, it is the type of the effect
  interpreter.

The `('R, +'e, +'a) s` type we saw earlier is the abstract effect signature
type. It represents the application `('e, 'a) 'R` of the higher-kinded `'R` type
constructor to the `'e` and `'a` arguments.

Now, let's interpret the effect reader type of our Fibonacci function:

```ml
  ('R, 'e, int, (('R, 'D) #applicative' as 'D)) er
```

First of all, we can see that it returns an `int` in case it does not fail with
an error. Speaking of which, the type of errors is `'e`, which, due to
parametricity, means that it cannot produce errors. In other words, we (and the
OCaml type system) statically know that it cannot fail. The representation
identification type `'R` is also parametric, which means that it does not
require a specific representation. The dictionary type `'D` is also parametric
and must be a subtype of `applicative'`. In other words, we can run the effect
reader with any interpreter that implements the `applicative'` effect
capabilities.

For example, we can use the identity monad implementation provided by the Rea
framework:

```ocaml
  let () = assert (55 = Identity.of_rea (run Identity.monad (eager 10)))
```

The identity monad does not perform any effects per se and the values computed
during interpretation are not wrapped. In other words, with the identity monad,
`('R, 'e, 'a) s` is equivalent to `'a`. This equivalence is witnessed by a pair
of identity functions `Identity.of_rea` and `Identity.to_rea`. By itself, the
identity monad cannot support the error handling effects of the Rea framework
nor it can support asynchronous computations. It may seem like a useless
interpreter, but it actually has many interesting applications.

The `run` function used above just passes the interpreter to the computation.
One could also just write `eager 10 Identity.monad`.

We can also use the self tail recursive (and
[Js_of_ocaml](https://ocsigen.org/js_of_ocaml/latest/manual/overview)
[safe](https://ocsigen.org/js_of_ocaml/latest/manual/tailcall)) interpreter also
provided by the Rea framework:

```ocaml
  let () = assert (`Ok 55 = Tailrec.run Tailrec.sync (eager 10))
```

The tail recursive interpreter provides both a synchronous, as seen above, and
an asynchronous interpreter and also supports error handling. The Rea framework
also provides a number of other interpreter implementations, that we could use
here, but let's move on.

At the beginning we mentioned that the `eager` function is written naïvely. The
problem with it is that it is eager: as soon as the first argument is passed to
it, a whole tree of suspended computations is built.

Now, we saw that the result of `eager n` is actually a function &mdash; namely
an effect reader. Because it is a function, we can use (eta) η-expansion to make
it lazy. For this purpose the Rea framework provides a simple `eta'0` function
that takes a thunk and returns a single parameter function. Using `eta'0` we can
write an η-expanded Fibonacci function as follows:

```ocaml
  let rec inert n = eta'0 @@ fun () ->
    if n <= 1 then
      pure n
    else
      lift'2 ( + ) (inert (n - 2)) (inert (n - 1))
```

The `inert` and `eager` functions have exactly the same types. The difference is
that the η-expanded `inert` function returns in O(1) time with a function

```ml
  inert n = fun d -> ...
```

while the `eager` function builds a complete computation tree

```ml
  eager 0 = pure 0
  eager 1 = pure 1
  eager 2 = lift'2 (+) (pure 0) (pure 1)
  eager 3 = lift'2 (+) (pure 1) (lift'2 (+) (pure 0) (pure 1))
  eager 4 = lift'2 (+) (lift'2 (+) (pure 0) (pure 1))
                           (lift'2 (+) (pure 1) (lift'2 (+) (pure 0) (pure 1)))
  ...
```

taking exponential time and space.

Of course, when either one of the effect readers is interpreted, it will take
exponential time due to the naïve exponential Fibonacci algorithm. Again, the
difference is that one of the computations is generated lazily on demand while
the other is generated eagerly.

Just like with the previous `eager`, we can use multiple interpreters to run
`inert` computations:

```ocaml
  let () = assert (55 = Identity.of_rea (run Identity.monad (inert 10)))
  let () = assert (`Ok 55 = Tailrec.run Tailrec.sync (inert 10))
```

This concludes the Fibonacci example.

```ocaml
end
```

We now have a basic understanding of effect readers. They are just functions
that take a dictionary of capabilities aka an interpreter as an argument.

### Extensible environment

Let's continue with an example demonstrating the extensible environment of the
Rea approach. To do so, let's implement a very rudimentary interpreter using a
modular approach. Although the techniques in this section scale to more
interesting language processors, we will keep the example very minimal.

First we'll implement a simple arithmetic language:

```ocaml
module Num = struct
```

For later use, we'll define a structural type matching the arithmetic language:

```ocaml
  type 't t =
    [`Num of int | `Uop of [`Neg] * 't | `Bop of [`Add | `Mul] * 't * 't]
```

Like in

> [Code reuse through polymorphic variants](https://www.math.nagoya-u.ac.jp/~garrigue/papers/variant-reuse.pdf)
> by Jacques Garrigue

we use polymorphic variants with open recursion for the AST representation.

We will also use open recursion in the `eval` functions:

```ocaml
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
```

The `eta'1` combinator is another way to write η-expanded functions. It takes a
function and returns a two parameter function. The `>>=` aka `bind` aka `let*`
is for monadic bind and `<*>` aka `pair` aka `let+ ... and+ ...` is for
applicative pairing of computation.

Errors are reported with the `fail` combinator. We use polymorphic variants for
errors.

After some cleaning up, the type inferred for `eval` is roughly equivalent to
the following definition:

```ocaml
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
```

Notice that the above type shows both of the errors that might arise from
`eval`.

The `sync'` class is a combination of `monad'` and `errors'` and `errors'` is a
combination of`fail'` and `tryin'`. In other words, it provides both the basic
monadic capabilities for sequencing and the ability to signal and handle errors.

```ocaml
end
```

Let's then move on to implement (lambda) λ-expressions:

```ocaml
module Lam = struct
```

Like with the arithmetic language we define a type for the language:

```ocaml
  module Id = String

  type 't t = [`Lam of Id.t * 't | `App of 't * 't | `Var of Id.t]
```

Deviating from Garrigue's example, we'll use an environment of bindings

```ocaml
  module Bindings = Map.Make (Id)
```

that maps variables to values. Recall that the arithmetic language above knows
nothing about environments. To pass around the environment we'll use the
extensible environment of the Rea framework. For that we define a new class
`['v] bindings` that exposes a `bindings` property:

```ocaml
  class ['v] bindings :
    object
      method bindings : 'v Bindings.t Prop.t
    end =
    object
      val mutable v : 'v Bindings.t = Bindings.empty
      method bindings = Prop.make (fun () -> v) (fun x -> v <- x)
    end
```

The `Prop.t` type, whose values are introduced by `Prop.make`, is provided by
the Rea framework to concisely expose a mutable instance variable as a readable
and functionally updatable property. To be clear, it is not actually possible to
observably mutate the `v` instance variable outside of the `bindings` class.

For easy access to the `bindings` method we define a trivial extractor:

```ocaml
  let bindings d = d#bindings
```

Now we are ready to write the open `eval` function for λ-expressions:

```ocaml
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
```

The `let+` binding operator is the `map` operation of functors. The `get`
combinator reads the value of a property extracted from the environment. The
`setting` combinator runs a computation with the value of a property
functionally updated to the given value. The `get_as` combinator reads a
property and also maps it through the given function.

The following definition shows a cleaned up type for the `eval` function:

```ocaml
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
```

Notice the `bindings` as part of the `'D` dictionary of capabilities.

```ocaml
end
```

Moving on to compose the full interpreter

```ocaml
module Full = struct
```

from the above parts, we write a recursive `eval` function that dispatches to
one of the above `eval` functions depending on the input:

```ocaml
  let rec eval = function
    | #Num.t as e -> Num.eval eval e
    | #Lam.t as e -> Lam.eval eval e
```

Again, here is a cleaned up type for the `eval` function:

```ocaml
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
```

Notice how the type combines

- the arithmetic and λ-expressions (as `'t`),
- the errors (`[> ...]`),
- the value type (as `'v`), and
- the capability dictionaries (as `'D`).

To actually run `eval` we will need an effect interpreter that provides the
`sync'` capabilities as well as `bindings`. There is an interpreter for the
standard `result` type that we can use for the `sync'` capabilities. For the
`bindings` we can just use `bindings`. Here is how:

```ocaml
  let () =
    assert (
      Error (`Error_unbound_var "y")
      = StdRea.Result.of_rea
          (run
             (object
                inherit [_] StdRea.Result.monad_errors
                inherit [_] Lam.bindings
             end)
             (eval (`App (`Lam ("x", `Bop (`Add, `Num 2, `Var "y")), `Num 1)))))
```

Another interpreter that comes bundled with the Rea framework that provides
`sync'` is the `Tailrec` interpreter we used previously. So, we could also use
`Tailrec` as follows:

```ocaml
  let () =
    assert (
      `Ok (`Num 3)
      = Tailrec.run
          (object
             inherit [_] Tailrec.sync
             inherit [_] Lam.bindings
          end)
          (eval (`App (`Lam ("x", `Bop (`Add, `Num 2, `Var "x")), `Num 1))))
```

We could also use the asynchronous version of the `Tailrec` interpreter. To
ensure that neither errors nor results are implicitly ignored, the
`Tailrec.spawn` function requires that the computation throws `nothing` and
returns `()`. We need to wrap the computation with handlers:

```ocaml
  let () =
    let result = ref @@ Ok (`Num 0) in
    Tailrec.spawn
      (object
         inherit [_] Tailrec.async
         inherit [_] Lam.bindings
      end)
      (eval (`App (`Lam ("x", `Bop (`Add, `Num 2, `Var "x")), `Num 1))
      |> tryin
           (fun e -> pure (result := Error e))
           (fun v -> pure (result := Ok v)));
    assert (!result = Ok (`Num 3))
```

The `tryin` combinator allows us to handle errors and continue with results.
Since we handle all of the errors, the error type for the whole computation
becomes parametric and can be unified with `nothing`.

Normally one cannot assume that a computation started with `Tailrec.spawn`
completes immediately. In this case we had nothing asynchronous in the
implementation and nothing asynchronous running in the background.

```ocaml
end
```

This concludes the example. We now know about the extensible environment as well
as about error handling.

### Projections

In the previous section we wrote a simple modular interpreter that used the
extensible environment of Rea to pass along the bindings capability. Adding to
the extensible environment is easy &mdash; you just declare what you want. In a
more complex program different parts of the program might require different
capabilities. The top-level of the program then ends up having to know about
about all of those capabilities. This is a modularity problem.

Imagine using a library using the framework. A new version of the library comes
along and your program no longer compiles just because the library now
internally needs a different set of capabilities compared to the previous
version. That is not good. We need a way to handle effects locally. Ideally we'd
like to be able to say that a program requires a specific set of capabilities to
run and also requires the freedom to extend the environment with some other
capabilities (i.e. that it "lacks" or is "disjoint" from the additional
capabilities). This way the environment could be efficiently extended using some
form of polymorphic record extension. Unfortunately OCaml's objects do not offer
such a form of polymorphism. What can we do?

OCaml does offer half of what we need: we can declare that we need at least some
specific capabilities from the environment. What we can do then is to project
those capabilities out of the environment and build our own scoped environment.

```ocaml
module Scoped = struct
```

Let's see how that is done. Here is an `eval` function that does not require
`bindings` from the environment:

```ocaml
  let eval e =
    Full.eval e
    |> mapping_env @@ fun o ->
       object
         inherit [_, _, _] sync'of o
         inherit [_] Lam.bindings
       end
```

The `mapping_env` combinator allows us to get the outer environment `o` and
substitute our own. For that we use the `sync'of` class above. It is given an
object that must be of some subtype of `sync'`. It then provides the `sync'`
capabilities by delegating to the given object. In other words, we project the
`sync'` capability out of the environment `o`.

The following definition shows a cleaned up type for the closed `eval`:

```ocaml
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
          (('R, 'D) #sync' as 'D) )
        er)
```

The `bindings` capability no longer appears in the environment type `'D` and we
can run it with just the base `Tailrec` interpreter:

```ocaml
  let () =
    assert (
      `Ok (`Num 42)
      = Tailrec.run Tailrec.sync
          (eval (`App (`Lam ("x", `Bop (`Add, `Num 2, `Var "x")), `Num 40))))
```

Being able to scope effects in this fashion is important for modularity.
Unfortunately doing so is not free as each projection adds some delegation
overhead to the effect invocations. Also, what we dealth with above is the easy
case where we modularized a first-order function. Higher-order functions where
the caller supplied functions also need to use the environment require wrapping
the user supplied functions replacing the environment back to what it was.

```ocaml
end
```

This concludes the example. We now know more about the extensible environment.

### Interoperability

Let's suppose next that we have an existing monadic library that we need to
interoperate with. For the sake of argument, let's assume the following
continuation monad implementation:

```ocaml
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
```

First we notice that we ran into a bit of a snag. Although Rea provides a number
of related effects, at the time of writing, no `callcc` effect is provided.
Fortunately nothing prevents users from extending the framework. We just write
down the `callcc'` class:

```ocaml
class virtual ['R, 'D] callcc' =
  object
    method virtual callcc'
        : 'e 'f 'a 'b.
          (('a -> ('R, 'f, 'b, 'D) er) -> ('R, 'e, 'a, 'D) er) -> ('R, 'e, 'a) s
  end
```

And the generic effect reader combinator `callcc`:

```ocaml
let callcc f (d: (_, _) #callcc') = d#callcc' f
```

Now, how do we relate `'a Cont.t` with the Rea framework? What we need to do is
to embed it into the abstract `('R, 'e, 's) s` effect signature type. We create
an abstract representation type `r` corresponding to the `Cont.t` type
constructor and an injection projection pair:

```ocaml
module ContRea = struct
  type r

  external to_rea : 'a Cont.t -> (r, 'e, 'a) s = "%identity"
  external of_rea : (r, 'e, 'a) s -> 'a Cont.t = "%identity"
```

Rea probably should provide a functor for the above, but it currently doesn't.
The special

```ml
  external fn : s -> t = "%identity"
```

construct tells OCaml that `fn` is an external function of type `s -> t` that is
actually the identity function. Because both the above type `r` and the type
`(_, _, _) s` from Rea are abstract, the above two coercions are safe &mdash; as
long as we don't provide other incompatible coercions between the abstract
types.

Now we can write down an interpreter class

```ocaml
  class ['D] monad_callcc =
    object (d: 'D)
      inherit [r, 'D] monad'd
      method pure' x = to_rea (Cont.return x)

      method bind' x f =
        to_rea (Cont.bind (of_rea (x d)) (fun x -> of_rea (f x d)))

      inherit [r, 'D] callcc'

      method callcc' f =
        to_rea (Cont.callcc (fun k -> of_rea (f (fun x _ -> to_rea (k x)) d)))
    end
```

based on `Cont`. As an example, we can use it with the previously defined
Fibonacci computations:

```ocaml
  let () =
    assert (55 = Cont.run (of_rea (run (new monad_callcc) (Fib.inert 10))))
```

And `callcc` is also available:

```ocaml
  let () =
    assert (
      101 = Cont.run (of_rea (run (new monad_callcc) (callcc (fun k -> k 101)))))
```

All the generic combinators for monads and simpler functors, and the extensible
environment are now available when constructing `Cont` computations via the Rea
framework.

```ocaml
end
```

We now know that we can write effect interpreters using existing monadic
libraries that run their computations embedded into the Rea framework and that
we can also introduce new effects into the framework.

### Traversals

Wonder what the last example will be about?

> The answer is always traverse. —
> [Rúnar](https://twitter.com/runarorama/status/1237817671619686400)

Indeed, mapping with an effect reader, `map_er`, or "traverse" as it is often
called, tends to be the answer to many problems. So, let's explore the topic a
bit. We'll build on the earlier modular interpreter example.

```ocaml
module Answer = struct
```

Although we could use a modular approach and build traversal functions modularly
for expressions, that's not really the point here. So, let's just work on the
full AST. Here is a generic traversal function for the structural AST:

```ocaml
  let map_er' nE o1E o2E iE eE = eta'1 @@ function
    | `Num x -> map_er'1 nE        x >>- fun x -> `Num x
    | `Uop x -> map_er'2 o1E eE    x >>- fun x -> `Uop x
    | `Bop x -> map_er'3 o2E eE eE x >>- fun x -> `Bop x
    | `Lam x -> map_er'2 iE eE     x >>- fun x -> `Lam x
    | `App x -> map_er'2 eE eE     x >>- fun x -> `App x
    | `Var x -> map_er'1 iE        x >>- fun x -> `Var x
```

Instead of just traversing over the direct subexpressions of an expression, the
above `map_er'` also allows traversing over the numbers, unary and binary
operators, and identifiers. The constructors of the datatype are traversed in a
systematic way using a family of canned `map_er'n` functions for each arity of a
tuple the constructors are carrying.

Why call it `map_er` rather than `traverse`? Well, mapping over a datatype with
an effect constructor is just one of many useful generalizations of ordinary
pure functions:

- `map` -> `map_er`
- `find` -> `find_er`
- `fold` -> `fold_er`
- ...

Systematic naming hopefully makes the API easier to learn.

I find it convenient to implement traversal for a structural type in the above
manner as it allows one to easily specialize to the basic traversal over
subexpressions

```ocaml
  let map_er eE = map_er' pure pure pure pure eE
```

and also use the generic traverse for other purposes.

The type of the `map_er` over subexpressions is seen in the below definition:

```ocaml
  type 't t = [ 't Num.t | 't Lam.t ]

  let _ =
    (map_er
      : ('s -> ('R, 'e, 't, (('R, 'D) #applicative' as 'D)) er) ->
        [< 's t] ->
        ('R, 'e, [> 't t], 'D) er)
```

What can one do with traversals? Well, a lot of things.

For example, here is a function that recursively traverses an expression to find
out whether a given variable is free or not in a given expression:

```ocaml
  let rec is_free i' = function
    | `Var i -> i = i'
    | `Lam (i, _) when i = i' -> false
    | e -> Traverse.to_exists map_er (is_free i') e

  let () = assert (is_free "y" (`App (`Lam ("x", `Var "x"), `Var "y")))
  let () = assert (not (is_free "x" (`App (`Lam ("x", `Var "x"), `Var "y"))))
```

Using `map_er` we can treat all the "basic" cases of the datatype generically
and focus on the two cases that are interesting with respect to bindings.

But we kind of got ahead of ourselves. How does `Traverse.to_exists` work?

Well, what `map_er` does is that it maps every element of a datatype to an
effect and then combines those effects to reconstruct the shape of the datatype.
If, for example, we use the previously mentioned `Identity` monad to run the
effects, then we basically get a `map` function for the datatype.

Effects are not limited to simply returning a value of the answer type. We
already previously saw the `fail` effect, which doesn't return an answer.
Another example is the `Constant` functor, which, as its name suggests, carries
along a constant value of some type and the answer type is a phantom type. The
`Constant` functor can be augmented to an applicative over a monoid. For
example, we can use disjunction. And that is what `Traverse.to_exists` does. It
maps the traversed elements to boolean constants as returned by the given
predicate. Then those booleans are combined using disjunction.

But this is all quite common knowledge. Why are we discussing this? Well, recall
that the Rea framework provides a form of laziness via η-expansion. That same
laziness also works with traversals over monoids, among other things. So, when
we call `Traverse.to_exists map_er (is_free i') e`, instead of first eagerly
building the computation for the whole expression tree `e`, as one might expect
to be the case in a strict language like OCaml, the computation is built on
demand and actually stops roughly as soon as the first free variable occurrence
has been encountered. So, although the use of objects and whatnot brings quite a
bit of overhead, at least we actually get the desired asymptotic time complexity
for `is_free`.

This concludes the example and the introduction.

```ocaml
end
```

We now know about most aspects of the Rea framework. Perhaps the best way to
learn more is to take a brief look at the
[reference manual](https://polytypic.github.io/rea-ml/rea/Rea/index.html) and
start using the framework. Have fun!

## Limitations

The main drawbacks of this approach come from the limitations of OCaml's
objects:

- OCaml does not aggressively optimize (statically known) method invocations.
  This means that every effect invocation has some overhead.

- OCaml's object system does not support adding methods to or removing methods
  from objects (i.e. polymorphic record extension). This means that effects
  cannot be easily handled locally.

On the other hand, this approach arguably has a rather straightforward
implementation and is convenient to use (modulo the inability to handle effects
locally).

Unfortunately the library
[reference manual](https://polytypic.github.io/rea-ml/rea/Rea/index.html) is
rather unfinished at the moment.
