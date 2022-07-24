open Signatures
open Derived

type r

external to_rea : 'a -> (r, 'e, 'a) s = "%identity"
external of_rea : (r, 'e, 'a) s -> 'a = "%identity"

class ['D] monad =
  object (d : 'D)
    inherit [r, 'D] monad'd
    method pure' = to_rea
    method bind' xI xyI = xyI (of_rea (xI d)) d
  end

let monad = new monad
