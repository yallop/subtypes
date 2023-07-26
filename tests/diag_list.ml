open Subtypes.Diag

(* Example *)
module PosList = struct type 'a t = 'a list end
module PosSnd = struct type 'a t = int * 'a end
let apply : type a b . (a, b) Sub.t -> (int * a) list -> (int * b) list =
  fun s ->
    coerce (lift {PosList} (lift {PosSnd} s))

