open Subtypes.Diag

type z = [`Zero]

type +'n suc = [`Succ of 'n | z]

(** Stuff about natural numbers *)
let z_sub_one : (z, z suc) sub =
  (refl : (z, z) sub :> (z, z suc) sub)

let inductive_step : 'n. ('n, 'n suc) sub -> ('n suc, 'n suc suc) sub =
  fun sub -> let module M = struct type +'a t = 'a suc end in
    liftPos {M} sub

type 'n nat =
    Z : z nat
  | S : 'n nat -> 'n suc nat

(** We /could/ add nat_sub as a primitive.  Or can we derive it
    generically? *)
let rec nat_sub : type n. n nat -> (n, n suc) sub = function
    Z -> z_sub_one
  | S n -> inductive_step (nat_sub n)

(** fin and weaken *)
type +_ fin =
    Z : ('n suc, 'm) sub -> 'm fin
  | S : ('n suc, 'm) sub * 'n fin -> 'm fin

module Fin = struct type 'a t = 'a fin end

let weaken : type n. n nat -> n fin -> n suc fin =
  fun nat s ->  (s >: liftPos {Fin} (nat_sub nat))

module M =
struct
  module Nat : sig
    type z and +'n suc
    val sub : ('n, 'n suc) sub
  end = struct
    type z = unit
    type +'n suc = 'n
    let sub = refl
  end
  let weaken s = (s >: liftPos {Fin} Nat.sub)

end
