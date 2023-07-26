(* This requires 4.02.0+modular-implicits or similar *)

(** Subtyping witnesses are the initial algebra over the following signature: *)
module type DIAG = sig
  type (-'a, +'b) t
  val refl : ('a, 'a) t
end


(** That is, Sub is DIAG... *)
module Sub : DIAG with type ('a, 'b) t = {D : DIAG} -> ('a, 'b) D.t

(** ... and maps into any other DIAG *)
val elim : {D : DIAG} -> ('a, 'b) Sub.t -> ('a, 'b) D.t


(** An example of a DIAG-algebra: functions *)
module Function : DIAG with type ('a, 'b) t = 'a -> 'b

(** Initiality means we can map Sub to Function, giving subtyping coercions *)
val coerce : ('a, 'b) Sub.t -> 'a -> 'b


(** We can lift a DIAG-algebra via any positive context *)
module type POS = sig type +'a t end
module Lift (P : POS) (D : DIAG) : DIAG with type ('a, 'b) t = ('a P.t, 'b P.t) D.t

(** Initiality means we can map Sub to Lift P Sub, giving covariance of witnesses *)
val lift : {P : POS} -> ('a, 'b) Sub.t -> ('a P.t, 'b P.t) Sub.t


(** This definition is an alternative to subtyping witnesses defined by contexts *)
module CtxSub : DIAG with type (-'a, +'b) t = {P:POS} -> ('a P.t -> 'b P.t)

(* We can convert back and forth between both types of witness *)
val ctxsub_of_sub : ('a, 'b) Sub.t -> ('a, 'b) CtxSub.t
val sub_of_ctxsub : ('a, 'b) CtxSub.t -> ('a, 'b) Sub.t

module Id : sig type 'a t = 'a end

module Compose(F:POS)(G:POS) : sig type 'a t = 'a F.t G.t end

type (-'a, +'b) sub = {P:POS} -> ('a P.t -> 'b P.t)

val refl : ('a, 'a) sub

val liftPos : {P:POS} -> ('a,'b) sub -> ('a P.t,'b P.t) sub

val (>:) : 'a -> ('a, 'b) sub -> 'b

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


(* Some extra operators *)

(* positive contexts *)
module type NEG = sig type -'a t end

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

                           (* Conversions between the various encodings.*)

(* We can convert between any two implementations of SUB. *)
val sub_of_sub : {B: SUB} -> {A:SUB} -> ('a, 'b) A.t -> ('a, 'b) B.t

type (-'a, +'b) sub_diag = {D : DIAG} -> ('a, 'b) D.t

module Diag : SUB with type ('a, 'b) t = ('a, 'b) sub_diag

(* pos *)
type (-'a, +'b) sub_pos = {P:POS} -> ('a P.t -> 'b P.t)

module Pos : SUB with type ('a, 'b) t = ('a, 'b) sub_pos

(* neg *)
type (-'a, +'b) sub_neg = {N:NEG} -> ('b N.t -> 'a N.t)

(* conversions *)
val pos_of_neg : ('a, 'b) sub_neg -> ('a, 'b) sub_pos

val diag_of_pos : ('a, 'b) sub_pos -> ('a, 'b) sub_diag

val neg_of_diag : ('a, 'b) sub_diag -> ('a, 'b) sub_neg

(* (the remainder can be obtained via composition) *)
val pos_of_diag : ('a, 'b) sub_diag -> ('a, 'b) sub_pos

val diag_of_neg : ('a, 'b) sub_neg -> ('a, 'b) sub_diag

val neg_of_pos : ('a, 'b) sub_pos -> ('a, 'b) sub_neg
