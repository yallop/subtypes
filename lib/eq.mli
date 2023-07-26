(* Removing variance annotations gives equality *)

(* contexts *)
module type CONTEXT = sig type 'a t end

(* diag, with no variance *)
module type DIAG = sig
  type ('a, 'b) t
  val refl : ('a, 'a) t
end

(* the basic subtyping interface, with no variance *)
module type EQ =
sig
  include DIAG
  val lift : {P:CONTEXT} -> ('a, 'b) t -> ('a P.t, 'b P.t) t
  val coerce : ('a, 'b) t -> 'a -> 'b
end

module Eq : EQ 

val symm : ('a, 'b) Eq.t -> ('b, 'a) Eq.t
