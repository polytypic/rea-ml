let ( >>> ) f g x = g (f x)
let ok a = `Ok a
let error e = `Error e
let flip = Fun.flip
let id = Fun.id
let const = Fun.const
