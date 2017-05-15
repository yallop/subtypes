(* This requires 4.02.0+modular-implicits or similar *) 

type z = [`Zero]

type +'n suc = [`Succ of 'n | z]

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

(** Stuff about natural numbers *)
let z_sub_one : (z, z suc) sub =
  (refl : (z, z) sub :> (z, z suc) sub)

let inductive_step : 'n. ('n, 'n suc) sub -> ('n suc, 'n suc suc) sub =
  fun sub -> let module M = struct type +'a t = 'a suc end in
    lift {M} sub

type 'n nat =
    Z : z nat
  | S : 'n nat -> 'n suc nat

(** We /could/ add nat_sub as a primitive.  Or can we derive it
    generically? *)
let rec nat_sub : type n. n nat -> (n, n suc) sub = function
    Z -> z_sub_one
  | S n -> inductive_step (nat_sub n)

(** fin and weaken *)
type +_ fin =
    Z : ('n suc, 'm) sub -> 'm fin
  | S : ('n suc, 'm) sub * 'n fin -> 'm fin

module Fin = struct type 'a t = 'a fin end

let weaken : type n. n nat -> n fin -> n suc fin =
  fun nat s ->  (s >: lift {Fin} (nat_sub nat))

module M =
struct
  module Nat : sig
    type z and +'n suc
    val sub : ('n, 'n suc) sub
  end = struct
    type z = unit
    type +'n suc = 'n
    let sub = refl
  end
  let weaken s = (s >: lift {Fin} Nat.sub)

end

module Neg =
struct
  module type NEG = sig type -'a t end

  (* module Id = struct type 'a t = 'a end *)

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

(* f {Id} x *)

end
