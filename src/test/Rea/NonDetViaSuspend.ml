open Rea

class virtual ['R, 'D] reject' =
  object
    method virtual reject' : 'e 'a. ('R, 'e, 'a) s
  end

let reject (d : (_, _) #reject') = d#reject'

class virtual ['R, 'D] choose' =
  object
    method virtual choose' : 'e 'a. 'a -> 'a -> ('R, 'e, 'a) s
  end

let choose a b (d : (_, _) #choose') = d#choose' a b

class virtual ['R, 'D] nondet' =
  object
    inherit ['R, 'D] reject'
    inherit ['R, 'D] choose'
  end

class virtual ['R, 'D] nondet'd =
  object (d : 'D)
    inherit ['R, 'D] suspend'
    inherit ['R, 'D] nondet'
    method reject' = d#suspend' ignore

    method choose' a b =
      d#suspend' @@ fun resume ->
      resume @@ `Ok b;
      resume @@ `Ok a
  end

let () =
  let events = ref [] in
  let push x = events := x :: !events in
  (let* x = choose "a" "b" in
   push x;
   let* y = choose "c" "d" in
   push y;
   reject)
  |> Tailrec.spawn
       (object
          inherit [_] Tailrec.async
          inherit [_, _] nondet'd
       end);
  assert (List.rev !events = ["a"; "c"; "d"; "b"; "c"; "d"])
