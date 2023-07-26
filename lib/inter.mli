(* Conversions between the various encodings.*)

(* positive contexts *)
module type POS = sig type +'a t end

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
  val coerce : ('a, 'b) t -> 'a -> 'b
end

(* We can convert between any two implementations of SUB. *)
val sub_of_sub : {B: SUB} -> {A:SUB} -> ('a, 'b) A.t -> ('a, 'b) B.t

type (-'a, +'b) sub_diag = {D : DIAG} -> ('a, 'b) D.t

module Diag : SUB with type ('a, 'b) t = ('a, 'b) sub_diag

(* pos *)
type (-'a, +'b) sub_pos = {P:POS} -> ('a P.t -> 'b P.t)

module Pos : SUB with type ('a, 'b) t = ('a, 'b) sub_pos

(* neg *)
module type NEG = sig type -'a t end
type (-'a, +'b) sub_neg = {N:NEG} -> ('b N.t -> 'a N.t)

module Neg : SUB with type ('a, 'b) t = ('a, 'b) sub_neg

(* conversions *)
val pos_of_neg : ('a, 'b) sub_neg -> ('a, 'b) sub_pos

val diag_of_pos : ('a, 'b) sub_pos -> ('a, 'b) sub_diag

val neg_of_diag : ('a, 'b) sub_diag -> ('a, 'b) sub_neg

(* (the remainder can be obtained via composition) *)
val pos_of_diag : ('a, 'b) sub_diag -> ('a, 'b) sub_pos

val diag_of_neg : ('a, 'b) sub_neg -> ('a, 'b) sub_diag

val neg_of_pos : ('a, 'b) sub_pos -> ('a, 'b) sub_neg
