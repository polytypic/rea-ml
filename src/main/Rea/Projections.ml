open Signatures

let arg'1 d xE _ = xE d
let arg'2 d xyE x _ = xyE x d

class ['R, 'O, 'D] map'of (o : ('R, 'O) #map') =
  object (d : 'D)
    inherit ['R, 'D] map'
    method map' xy xE = o#map' xy (arg'1 d xE)
  end

class ['R, 'O, 'D] pair'of (o : ('R, 'O) #pair') =
  object (d : 'D)
    inherit ['R, 'D] pair'
    method pair' xE yE = o#pair' (arg'1 d xE) (arg'1 d yE)
  end

class ['R, 'O, 'D] product'of (o : ('R, 'O) #product') =
  object (_ : 'D)
    inherit ['R, 'O, 'D] map'of o
    inherit ['R, 'O, 'D] pair'of o
  end

class ['R, 'O, 'D] pure'of (o : ('R, 'O) #pure') =
  object (_ : 'D)
    inherit ['R, 'D] pure'
    method pure' x = o#pure' x
  end

class ['R, 'O, 'D] branch'of (o : ('R, 'O) #branch') =
  object (d : 'D)
    inherit ['R, 'D] branch'

    method branch' xzE yzE xyE =
      o#branch' (arg'1 d xzE) (arg'1 d yzE) (arg'1 d xyE)
  end

class ['R, 'O, 'D] bind'of (o : ('R, 'O) #bind') =
  object (d : 'D)
    inherit ['R, 'D] bind'
    method bind' xE xyE = o#bind' (arg'1 d xE) (arg'2 d xyE)
  end

class ['R, 'O, 'D] pointed'of (o : ('R, 'O) #pointed') =
  object
    inherit ['R, 'O, 'D] map'of o
    inherit ['R, 'O, 'D] pure'of o
  end

class ['R, 'O, 'D] applicative'of (o : ('R, 'O) #applicative') =
  object
    inherit ['R, 'O, 'D] pointed'of o
    inherit ['R, 'O, 'D] pair'of o
  end

class ['R, 'O, 'D] selective'of (o : ('R, 'O) #selective') =
  object
    inherit ['R, 'O, 'D] applicative'of o
    inherit ['R, 'O, 'D] branch'of o
  end

class ['R, 'O, 'D] monad'of (o : ('R, 'O) #monad') =
  object (_ : 'D)
    inherit ['R, 'O, 'D] selective'of o
    inherit ['R, 'O, 'D] bind'of o
  end

class ['R, 'O, 'D] zero'of (o : ('R, 'O) #zero') =
  object (_ : 'D)
    inherit ['R, 'D] zero'
    method zero' = o#zero'
  end

class ['R, 'O, 'D] alt'of (o : ('R, 'O) #alt') =
  object (d : 'D)
    inherit ['R, 'D] alt'
    method alt' lE rE = o#alt' (arg'1 d lE) (arg'1 d rE)
  end

class ['R, 'O, 'D] plus'of (o : ('R, 'O) #plus') =
  object (_ : 'D)
    inherit ['R, 'O, 'D] zero'of o
    inherit ['R, 'O, 'D] alt'of o
  end

class ['R, 'O, 'D] fail'of (o : ('R, 'O) #fail') =
  object (_ : 'D)
    inherit ['R, 'D] fail'
    method fail' e = o#fail' e
  end

class ['R, 'O, 'D] tryin'of (o : ('R, 'O) #tryin') =
  object (d : 'D)
    inherit ['R, 'D] tryin'
    method tryin' exE yxE yE = o#tryin' (arg'2 d exE) (arg'2 d yxE) (arg'1 d yE)
  end

class ['R, 'O, 'D] errors'of (o : ('R, 'O) #errors') =
  object (_ : 'D)
    inherit ['R, 'O, 'D] fail'of o
    inherit ['R, 'O, 'D] tryin'of o
  end

class ['R, 'O, 'D] par'of (o : ('R, 'O) #par') =
  object (d : 'D)
    inherit ['R, 'D] par'
    method par' xE yE = o#par' (arg'1 d xE) (arg'1 d yE)
  end

class ['R, 'O, 'D] suspend'of (o : ('R, 'O) #suspend') =
  object (_ : 'D)
    inherit ['R, 'D] suspend'
    method suspend' on = o#suspend' on
  end

class ['R, 'O, 'D] spawn'of (o : ('R, 'O) #spawn') =
  object (d : 'D)
    inherit ['R, 'D] spawn'
    method spawn' uE = o#spawn' (arg'1 d uE)
  end

class ['R, 'O, 'D] sync'of (o : ('R, 'O) #sync') =
  object (_ : 'D)
    inherit ['R, 'O, 'D] monad'of o
    inherit ['R, 'O, 'D] errors'of o
  end

class ['R, 'O, 'D] async'of (o : ('R, 'O) #async') =
  object (_ : 'D)
    inherit ['R, 'O, 'D] sync'of o
    inherit ['R, 'O, 'D] suspend'of o
    inherit ['R, 'O, 'D] par'of o
    inherit ['R, 'O, 'D] spawn'of o
  end
