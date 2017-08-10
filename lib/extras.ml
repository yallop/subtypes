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

module Derived(S: SUB) =
struct
  open S
  type ('a, 'b) sub = ('a, 'b) S.t

  let trans : type a b c. (a, b) sub -> (b, c) sub -> (a, c) sub =
    fun x y ->
    let module M = struct type +'c t = (a,'c) sub end
    in x >: lift {M} y

  let liftNeg : type a b. {N:NEG} -> (a, b) sub -> (b N.t, a N.t) sub =
    fun {N:NEG} x ->
    let module M = struct type +'b t = ('b N.t, a N.t) sub end in
    refl >: lift {M} x

                            
end

(* let liftneg *)
