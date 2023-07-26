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
