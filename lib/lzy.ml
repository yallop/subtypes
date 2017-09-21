(* Example from Francois Pottier: with first-class subtypes
   user-defined lazy can be covariant rather than invariant.

   In OCaml this is pretty useful, since covariance supports
   extra polymorphism in 'let'.
*)

   

module Lzy (S: sig type (-_,+_) sub val refl : ('a,'a) sub val coerce : ('a,'b) sub -> 'a -> 'b end) :
sig
  type +_ t

  val lzy : (unit -> 'a) -> 'a t
  val fce : 'a t -> 'a
end =
struct
  open S

  type +'a lazy_cell =
    | Thunk of (unit -> 'a)
    | Value of 'a
    | Exception of exn

  type +_ t = L : ('a, 'b) sub * 'a lazy_cell ref -> 'b t

  let lzy f = L (refl, ref (Thunk f))

  let fce (L (sub, r)) =
    match !r with
    | Thunk f -> (match f () with
                  | v -> r := Value v; coerce sub v
                  | exception e -> r := Exception e; raise e)
    | Value v -> coerce sub v
    | Exception e -> raise e
end
