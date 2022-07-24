open Combinators
open Util

let to_map map_er xy =
  map_er (fun x _ -> Identity.to_rea (xy x))
  >>> run Identity.monad >>> Identity.of_rea

let to_set map_er = const >>> to_map map_er

(* *)

let to_map_constant map_er m xc =
  map_er (fun x _ -> Constant.to_rea (xc x)) >>> run m >>> Constant.of_rea

let to_get map_er = to_map_constant map_er Constant.functr id
let to_get_opt map_er = to_map_constant map_er Constant.option Option.some
let to_exists map_er = to_map_constant map_er Constant.disjunction
let to_find_map map_er = to_map_constant map_er Constant.option

let to_map_reduce map_er plus zero =
  to_map_constant map_er @@ new Constant.applicative zero plus

let to_iter_er map_er = to_map_constant map_er Constant.unit_er
let to_exists_er map_er = to_map_constant map_er Constant.disjunction_er
let to_find_map_er map_er = to_map_constant map_er Constant.option_er
