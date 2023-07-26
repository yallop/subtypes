(* Example from Francois Pottier: with first-class subtypes
   user-defined lazy can be covariant rather than invariant.

   In OCaml this is pretty useful, since covariance supports
   extra polymorphism in 'let'.
*)

module type LAZY =
sig
  type +_ t

  val delay : (unit -> 'a) -> 'a t
  val force : 'a t -> 'a
end


module Lazy_sub (S: Subtypes.SUB) : LAZY =
struct
  open S

  type +'a lazy_cell =
    | Thunk of (unit -> 'a)
    | Value of 'a
    | Exception of exn

  type +_ t = L : ('a, 'b) S.t * 'a lazy_cell ref -> 'b t

  let delay f = L (refl, ref (Thunk f))

  let force (L (sub, r)) =
    match !r with
    | Thunk f -> (match f () with
                  | v -> r := Value v; v >: sub
                  | exception e -> r := Exception e; raise e)
    | Value v -> v >: sub
    | Exception e -> raise e
end

module Lazy_thunk : LAZY =
struct
  type +'a lazy_cell =
    | Thunk of (unit -> 'a)
    | Value of 'a
    | Exception of exn

  type +'a t = (unit -> 'a)

  let delay f =
    let r = ref (Thunk f) in 
    fun () -> match !r with
      | Thunk f -> (match f () with
          | v -> r := Value v; v
          | exception e -> r := Exception e; raise e)
      | Value v -> v
      | Exception e -> raise e

  let force f = f ()
end
