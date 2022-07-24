type 'a t = 'a option -> 'a

let make get set = function
  | None -> get ()
  | Some x ->
    set x;
    x

let get f r = f r None

let set f x r =
  let r = Oo.copy r in
  f r @@ Some x |> ignore;
  r

let map f fn r =
  let r = Oo.copy r in
  f r @@ Some (fn (f r None)) |> ignore;
  r
