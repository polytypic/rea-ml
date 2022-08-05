module Prop = Prop

let prop = Prop.make

include Signatures
include Derived
include Projections
include Combinators
module Constant = Constant
module Identity = Identity
module Memo = Memo
module Mut = Mut
module Tailrec = Tailrec
include Mut.Syntax
module StdRea = StdRea
module Traverse = Traverse
