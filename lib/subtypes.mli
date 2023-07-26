(* This requires 4.02.0+modular-implicits or similar *)

(** Types *)

module type POS = sig type +'a t end
module type NEG = sig type -'a t end

(** Subtyping witnesses are the initial algebra over the following signature: *)
module type DIAG = sig
  type (-'a, +'b) t
  val refl : ('a, 'a) t
end

module Diag : sig
  include DIAG with type ('a, 'b) t = {D : DIAG} -> ('a, 'b) D.t
  (** Diag.t maps into any other DIAG *)
  val elim : {D : DIAG} -> ('a, 'b) t -> ('a, 'b) D.t
end

(** Another DIAG-algebra: functions *)
module Function : DIAG with type ('a, 'b) t = 'a -> 'b

(** We can also lift a DIAG-algebra via any positive context *)
module LiftDiag (P : POS) (D : DIAG) : DIAG with type ('a, 'b) t = ('a P.t, 'b P.t) D.t

(** The basic subtyping interface *)
module type SUB =
sig
  include DIAG
  val lift : {P:POS} -> ('a, 'b) t -> ('a P.t, 'b P.t) t
  val (>:) : 'a -> ('a, 'b) t -> 'b
end

(** We can convert between any two implementations of SUB. *)
val sub_of_sub : {B: SUB} -> {A:SUB} -> ('a, 'b) A.t -> ('a, 'b) B.t

(** The standard subtyping witness is Diag. *)
module Sub : SUB with type ('a, 'b) t = ('a, 'b) Diag.t

(** Alternatively, we can define subtyping witnesses via positive contexts *)
module PosSub : SUB with type (-'a, +'b) t = {P:POS} -> ('a P.t -> 'b P.t)

(** ... or via negative contexts *)
module NegSub : SUB with type ('a, 'b) t = {N:NEG} -> ('b N.t -> 'a N.t)

(** Top-level functions *)
val refl : {S:SUB} -> ('a, 'a) S.t
val trans : {S:SUB} -> ('a, 'b) S.t -> ('b, 'c) S.t -> ('a, 'c) S.t
val (>:) : {S:SUB} -> 'a -> ('a, 'b) S.t -> 'b
val liftP : {S:SUB} -> {P:POS} -> ('a, 'b) S.t -> ('a P.t, 'b P.t) S.t
val liftN : {S:SUB} -> {N:NEG} -> ('a, 'b) S.t -> ('b N.t, 'a N.t) S.t
