type 'a ok = [`Ok of 'a]
type 'e error = [`Error of 'e]
type ('l, 'r) branch = [`Fst of 'l | `Snd of 'r]
type ('e, 'a) res = ['a ok | 'e error]
type ('d, 'c) cps = ('d -> 'c) -> 'c
type 'a op'1 = 'a -> 'a
type 'a op'2 = 'a -> 'a -> 'a
type 'a lazy_op'2 = (unit -> 'a) -> (unit -> 'a) -> 'a

(* *)

type nothing = |

(* *)

type ('R, +'e, +'a) s
type ('R, 'e, 'a, 'D) er = 'D -> ('R, 'e, 'a) s

(* *)

type ('R, 'e, 'a, 'b, 'D) map'm =
  ('b -> 'a) -> ('R, 'e, 'b, 'D) er -> ('R, 'e, 'a) s

class virtual ['R, 'D] map' =
  object
    method virtual map' : 'e 'a 'b. ('R, 'e, 'a, 'b, 'D) map'm
  end

type ('R, 'e, 'a, 'D) pure'm = 'a -> ('R, 'e, 'a) s

class virtual ['R, 'D] pure' =
  object
    method virtual pure' : 'e 'a. ('R, 'e, 'a, 'D) pure'm
  end

type ('R, 'e, 'a, 'b, 'D) pair'm =
  ('R, 'e, 'a, 'D) er -> ('R, 'e, 'b, 'D) er -> ('R, 'e, 'a * 'b) s

class virtual ['R, 'D] pair' =
  object
    method virtual pair' : 'e 'a 'b. ('R, 'e, 'a, 'b, 'D) pair'm
  end

type ('R, 'e, 'a, 'b, 'c, 'D) branch'm =
  ('R, 'e, 'b -> 'a, 'D) er ->
  ('R, 'e, 'c -> 'a, 'D) er ->
  ('R, 'e, ('b, 'c) branch, 'D) er ->
  ('R, 'e, 'a) s

class virtual ['R, 'D] branch' =
  object
    method virtual branch' : 'e 'a 'b 'c. ('R, 'e, 'a, 'b, 'c, 'D) branch'm
  end

type ('R, 'e, 'a, 'b, 'D) bind'm =
  ('R, 'e, 'b, 'D) er -> ('b -> ('R, 'e, 'a, 'D) er) -> ('R, 'e, 'a) s

class virtual ['R, 'D] bind' =
  object
    method virtual bind' : 'e 'a 'b. ('R, 'e, 'a, 'b, 'D) bind'm
  end

type ('R, 'e, 'a, 'D) zero'm = ('R, 'e, 'a) s

class virtual ['R, 'D] zero' =
  object
    method virtual zero' : 'e 'a. ('R, 'e, 'a, 'D) zero'm
  end

type ('R, 'e, 'a, 'D) alt'm =
  ('R, 'e, 'a, 'D) er -> ('R, 'e, 'a, 'D) er -> ('R, 'e, 'a) s

class virtual ['R, 'D] alt' =
  object
    method virtual alt' : 'e 'a. ('R, 'e, 'a, 'D) alt'm
  end

type ('R, 'e, 'a, 'D) fail'm = 'e -> ('R, 'e, 'a) s

class virtual ['R, 'D] fail' =
  object
    method virtual fail' : 'e 'a. ('R, 'e, 'a, 'D) fail'm
  end

type ('R, 'e, 'f, 'a, 'b, 'D) tryin'm =
  ('f -> ('R, 'e, 'a, 'D) er) ->
  ('b -> ('R, 'e, 'a, 'D) er) ->
  ('R, 'f, 'b, 'D) er ->
  ('R, 'e, 'a) s

class virtual ['R, 'D] tryin' =
  object
    method virtual tryin' : 'e 'f 'a 'b. ('R, 'e, 'f, 'a, 'b, 'D) tryin'm
  end

type ('R, 'e, 'a, 'b, 'D) par'm = ('R, 'e, 'a, 'b, 'D) pair'm

class virtual ['R, 'D] par' =
  object
    method virtual par' : 'e 'a 'b. ('R, 'e, 'a, 'b, 'D) par'm
  end

type ('R, 'e, 'a, 'D) suspend'm = (('e, 'a) res, unit) cps -> ('R, 'e, 'a) s

class virtual ['R, 'D] suspend' =
  object
    method virtual suspend' : 'e 'a. ('R, 'e, 'a, 'D) suspend'm
  end

type ('R, 'e, 'D) spawn'm = ('R, nothing, unit, 'D) er -> ('R, 'e, unit) s

class virtual ['R, 'D] spawn' =
  object
    method virtual spawn' : 'e. ('R, 'e, 'D) spawn'm
  end

class virtual ['R, 'D] functr' =
  object
    inherit ['R, 'D] map'
  end

class virtual ['R, 'D] pointed' =
  object
    inherit ['R, 'D] map'
    inherit ['R, 'D] pure'
  end

class virtual ['R, 'D] product' =
  object
    inherit ['R, 'D] map'
    inherit ['R, 'D] pair'
  end

class virtual ['R, 'D] applicative' =
  object
    inherit ['R, 'D] pointed'
    inherit ['R, 'D] pair'
  end

class virtual ['R, 'D] selective' =
  object
    inherit ['R, 'D] applicative'
    inherit ['R, 'D] branch'
  end

class virtual ['R, 'D] monad' =
  object
    inherit ['R, 'D] selective'
    inherit ['R, 'D] bind'
  end

class virtual ['R, 'D] plus' =
  object
    inherit ['R, 'D] zero'
    inherit ['R, 'D] alt'
  end

class virtual ['R, 'D] errors' =
  object
    inherit ['R, 'D] fail'
    inherit ['R, 'D] tryin'
  end

class virtual ['R, 'D] sync' =
  object
    inherit ['R, 'D] monad'
    inherit ['R, 'D] errors'
  end

class virtual ['R, 'D] async' =
  object
    inherit ['R, 'D] sync'
    inherit ['R, 'D] suspend'
    inherit ['R, 'D] par'
    inherit ['R, 'D] spawn'
  end
