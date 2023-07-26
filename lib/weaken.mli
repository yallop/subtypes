(* This requires 4.02.0+modular-implicits or similar *) 

type z = [`Zero]

type +'n suc = [`Succ of 'n | z]

module type POS = sig type +'a t end

module Id : sig type 'a t = 'a end

module Compose(F:POS)(G:POS) : sig type 'a t = 'a F.t G.t end

type (-'a, +'b) sub = {P:POS} -> ('a P.t -> 'b P.t)

val refl : ('a, 'a) sub

val lift : {P:POS} -> ('a,'b) sub -> ('a P.t,'b P.t) sub

val (>:) : 'a -> ('a, 'b) sub -> 'b

(** Stuff about natural numbers *)
val z_sub_one : (z, z suc) sub

val inductive_step : ('n, 'n suc) sub -> ('n suc, 'n suc suc) sub

type 'n nat =
    Z : z nat
  | S : 'n nat -> 'n suc nat

(** We /could/ add nat_sub as a primitive.  Or can we derive it
    generically? *)
val nat_sub : 'n nat -> ('n, 'n suc) sub

(** fin and weaken *)
type +_ fin =
    Z : ('n suc, 'm) sub -> 'm fin
  | S : ('n suc, 'm) sub * 'n fin -> 'm fin

module Fin : sig type 'a t = 'a fin end

val weaken : 'n nat -> 'n fin -> 'n suc fin

module M :
sig
  module Nat : sig
    type z and +'n suc
    val sub : ('n, 'n suc) sub
  end
  val weaken : 'a fin -> 'a Nat.suc fin
end

module Neg :
sig
  module type NEG = sig type -'a t end

  (* module Id = struct type 'a t = 'a end *)

  module Compose(F:POS)(G:NEG) :
    sig type 'a t = 'a F.t G.t end

  type (-'a, +'b) sub = {N:NEG} -> ('b N.t -> 'a N.t)

  val refl : ('a, 'a) sub

  val lift : {P:POS} -> ('a,'b) sub -> ('a P.t,'b P.t) sub

  val (>:) : 'a -> ('a, 'b) sub -> 'b
end
