(* Some extra operators *)

(* positive contexts *)
module type POS = sig type +'a t end
module type NEG = sig type -'a t end

(* diag *)
module type DIAG = sig
  type (-'a, +'b) t
  val refl : ('a, 'a) t
end

(* the basic subtyping interface *)
module type SUB =
sig
  include DIAG
  val lift : {P:POS} -> ('a, 'b) t -> ('a P.t, 'b P.t) t
  val (>:) : 'a -> ('a, 'b) t -> 'b
end

module Derived(S: SUB) : sig
  val trans : ('a, 'b) S.t -> ('b, 'c) S.t -> ('a, 'c) S.t
  val liftNeg : {N:NEG} -> ('a, 'b) S.t -> ('b N.t, 'a N.t) S.t
end

(* let liftneg *)
