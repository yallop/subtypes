(* This requires 4.02.0+modular-implicits or similar *) 

module type POS = sig type +'a t end

module Id : sig type 'a t = 'a end

module Compose(F:POS)(G:POS) : sig type 'a t = 'a F.t G.t end

type (-'a, +'b) sub = {P:POS} -> ('a P.t -> 'b P.t)

val refl : ('a, 'a) sub

val lift : {P:POS} -> ('a,'b) sub -> ('a P.t,'b P.t) sub

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
