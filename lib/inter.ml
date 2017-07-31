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
let sub_of_sub : type a b. {B: SUB} -> {A:SUB} ->
  (a, b) A.t -> (a, b) B.t =
  fun {B:SUB} {A:SUB} a ->
    let module P = struct type 'a t = (a, 'a) B.t end in
    A.coerce (A.lift {P} a) B.refl

type (-'a, +'b) sub_diag = {D : DIAG} -> ('a, 'b) D.t

module Diag : SUB with type ('a, 'b) t = ('a, 'b) sub_diag =
struct
  module Sub = struct
    type ('a, 'b) t = ('a, 'b) sub_diag
    let refl {D:DIAG} = D.refl
  end
  module Function = struct
    type ('a, 'b) t = 'a -> 'b
    let refl x = x
  end
  include Sub
  let elim {D:DIAG} (f : ('a, 'b) t) = f {D}
  module Lift (P : POS) (D : DIAG) = struct
    type ('a, 'b) t = ('a P.t, 'b P.t) D.t
    let refl = D.refl
  end
  let lift : type a b . {P : POS} -> (a, b) t -> (a P.t, b P.t) t =
    fun {P : POS} -> elim {Lift (P) (Sub)}
  let coerce : type a b . (a, b) Sub.t -> a -> b =
    fun x -> elim {Function} x
end

(* pos *)
type (-'a, +'b) sub_pos = {P:POS} -> ('a P.t -> 'b P.t)

module Pos : SUB with type ('a, 'b) t = ('a, 'b) sub_pos =
struct
  type ('a, 'b) t = ('a, 'b) sub_pos
  let refl : 'a. ('a, 'a) t =
    fun {P:POS} x -> x
      
  module Compose(F:POS)(G:POS) =
  struct type 'a t = 'a F.t G.t end
  
  module Id = struct type 'a t = 'a end

  let lift : 'a 'b. {P:POS} -> ('a,'b) t -> ('a P.t,'b P.t) t =
    fun {P:POS} s {Q:POS} x -> s {Compose(P)(Q)} x
        
  let coerce : 'a 'b. ('a, 'b) t -> 'a -> 'b =
    fun f x -> f {Id} x
end

(* neg *)
module type NEG = sig type -'a t end
type (-'a, +'b) sub_neg = {N:NEG} -> ('b N.t -> 'a N.t)

module Neg : SUB with type ('a, 'b) t = ('a, 'b) sub_neg =
struct
  type ('a, 'b) t = ('a, 'b) sub_neg
  let refl : 'a. ('a, 'a) t =
    fun {N:NEG} x -> x

  module Compose(F:POS)(G:NEG) =
  struct type 'a t = 'a F.t G.t end

  let lift : 'a 'b. {P:POS} -> ('a,'b) t -> ('a P.t,'b P.t) t =
    fun {P:POS} s {Q:NEG} x -> s {Compose(P)(Q)} x

  let id x = x

  let coerce : type a b. (a, b) t -> a -> b =
    fun f x -> let module M = struct type -'a t = 'a -> b end in
      f {M} id x
end

(* conversions *)
let pos_of_neg : type a b. (a, b) sub_neg -> (a, b) sub_pos =
  fun x -> sub_of_sub {Pos} {Neg} x

let diag_of_pos : type a b. (a, b) sub_pos -> (a, b) sub_diag =
  fun x -> sub_of_sub {Diag} {Pos} x

let neg_of_diag : type a b. (a, b) sub_diag -> (a, b) sub_neg =
  fun x -> sub_of_sub {Neg} {Diag} x

(* (the remainder can be obtained via composition) *)
let pos_of_diag : type a b. (a, b) sub_diag -> (a, b) sub_pos =
  fun d -> pos_of_neg (neg_of_diag d)

let diag_of_neg : type a b. (a, b) sub_neg -> (a, b) sub_diag =
  fun n -> diag_of_pos (pos_of_neg n)

let neg_of_pos : type a b. (a, b) sub_pos -> (a, b) sub_neg =
  fun p -> neg_of_diag (diag_of_pos p)
