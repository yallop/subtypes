(* This requires 4.02.0+modular-implicits or similar *) 

module type POS = sig type +'a t end

module Id = struct type 'a t = 'a end

module Compose(F:POS)(G:POS) =
  struct type 'a t = 'a F.t G.t end

type (-'a, +'b) sub = {P:POS} -> ('a P.t -> 'b P.t)

let refl : 'a. ('a, 'a) sub =
  fun {P:POS} x -> x

let lift : 'a 'b. {P:POS} -> ('a,'b) sub -> ('a P.t,'b P.t) sub =
  fun {P:POS} s {Q:POS} x -> s {Compose(P)(Q)} x

let (>:) : 'a 'b. 'a -> ('a, 'b) sub -> 'b =
  fun x f -> f {Id} x

module Neg =
struct
  module type NEG = sig type -'a t end

  module Compose(F:POS)(G:NEG) =
    struct type 'a t = 'a F.t G.t end

  type (-'a, +'b) sub = {N:NEG} -> ('b N.t -> 'a N.t)

  let refl : 'a. ('a, 'a) sub =
    fun {N:NEG} x -> x

  let lift : 'a 'b. {P:POS} -> ('a,'b) sub -> ('a P.t,'b P.t) sub =
    fun {P:POS} s {Q:NEG} x -> s {Compose(P)(Q)} x

  let id x = x

  let (>:) : 'a 'b. 'a -> ('a, 'b) sub -> 'b =
    fun (type a) (type b) x (f : (a, b) sub) ->
      let module M = struct type -'a t = 'a -> b end in
      f {M} id x
end
