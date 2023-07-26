open Subtypes

type z = [`Zero]

type +'n suc = [`Succ of 'n | z]

(** Stuff about natural numbers *)
let z_sub_one : (z, z suc) Sub.t =
  (Sub.refl : (z, z) Sub.t :> (z, z suc) Sub.t)

let inductive_step : 'n. ('n, 'n suc) Sub.t -> ('n suc, 'n suc suc) Sub.t =
  fun sub -> let module M = struct type +'a t = 'a suc end in
    Sub.lift {M} sub

type 'n nat =
    Z : z nat
  | S : 'n nat -> 'n suc nat

(** We /could/ add nat_sub as a primitive.  Or can we derive it
    generically? *)
let rec nat_sub : type n. n nat -> (n, n suc) Sub.t = function
    Z -> z_sub_one
  | S n -> inductive_step (nat_sub n)

(** fin and weaken *)
type +_ fin =
    Z : ('n suc, 'm) Sub.t -> 'm fin
  | S : ('n suc, 'm) Sub.t * 'n fin -> 'm fin

module Fin = struct type 'a t = 'a fin end

let weaken : type n. n nat -> n fin -> n suc fin =
  fun nat s ->  Sub.(s >: lift {Fin} (nat_sub nat))

module M =
struct
  module Nat : sig
    type z and +'n suc
    val sub : ('n, 'n suc) Sub.t
  end = struct
    type z = unit
    type +'n suc = 'n
    let sub = Sub.refl
  end
  let weaken s = Sub.(s >: lift {Fin} Nat.sub)
end
