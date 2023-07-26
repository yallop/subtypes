(* This requires 4.02.0+modular-implicits or similar *)

(*
 * Subtyping witnesses are the initial algebra over the following signature:
 *)
module type DIAG = sig
  type (-'a, +'b) t
  val refl : ('a, 'a) t
end

(* That is, Sub is DIAG... *)
module Sub = struct
  type ('a, 'b) t = {D : DIAG} -> ('a, 'b) D.t
  let refl {D:DIAG} = D.refl
end

(* ... and maps into any other DIAG *)
let elim {D:DIAG} (f : ('a, 'b) Sub.t) = f {D}



(* An example of a DIAG-algebra: functions *)
module Function = struct
  type ('a, 'b) t = 'a -> 'b
  let refl x = x
end

(* Initiality means we can map Sub to Function, giving subtyping coercions *)
let coerce : type a b . (a, b) Sub.t -> a -> b =
  fun x -> elim {Function} x



(* We can lift a DIAG-algebra via any positive context *)
module type POS = sig type +'a t end
module Lift (P : POS) (D : DIAG) = struct
  type ('a, 'b) t = ('a P.t, 'b P.t) D.t
  let refl = D.refl
end

(* Initiality means we can map Sub to Lift P Sub, giving covariance of witnesses *)

let lift : type a b . {P : POS} -> (a, b) Sub.t -> (a P.t, b P.t) Sub.t =
  fun {P : POS} -> elim {Lift (P) (Sub)}


(* This definition is an alternative to subtyping witnesses defined by contexts *)
module CtxSub = struct
  type (-'a, +'b) t = {P:POS} -> ('a P.t -> 'b P.t)
  let refl : 'a. ('a, 'a) t =
    fun {P:POS} x -> x
end

(* We can convert back and forth between both types of witness *)

let ctxsub_of_sub : type a b . (a, b) Sub.t -> (a, b) CtxSub.t =
  fun s -> elim {CtxSub} s

let sub_of_ctxsub : type a b . (a, b) CtxSub.t -> (a, b) Sub.t =
  fun s ->
  let module P = struct type 'b t = (a, 'b) Sub.t end in
  s {P} Sub.refl

module Id = struct type 'a t = 'a end

module Compose(F:POS)(G:POS) =
  struct type 'a t = 'a F.t G.t end

type (-'a, +'b) sub = {P:POS} -> ('a P.t -> 'b P.t)

let refl : 'a. ('a, 'a) sub =
  fun {P:POS} x -> x

let liftPos : 'a 'b. {P:POS} -> ('a,'b) sub -> ('a P.t,'b P.t) sub =
  fun {P:POS} s {Q:POS} x -> s {Compose(P)(Q)} x

let (>:) : 'a 'b. 'a -> ('a, 'b) sub -> 'b =
  fun x f -> f {Id} x

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

(* We can convert between any two implementations of SUB. *)
let sub_of_sub : type a b. {B: SUB} -> {A:SUB} ->
  (a, b) A.t -> (a, b) B.t =
  fun {B:SUB} {A:SUB} a ->
    let module P = struct type 'a t = (a, 'a) B.t end in
    A.(>:) B.refl (A.lift {P} a) 

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
  let (>:) : type a b . a -> (a, b) Sub.t -> b =
    fun  (y : a) (x : (a,b) Sub.t) -> (elim x {Function} y : b)
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
        
  let (>:) : 'a 'b. 'a -> ('a, 'b) t -> 'b =
    fun x f -> f {Id} x
end

(* neg *)
type (-'a, +'b) sub_neg = {N:NEG} -> ('b N.t -> 'a N.t)
module Neg =
struct
  module type NEG = sig type -'a t end
  type ('a, 'b) t = ('a, 'b) sub_neg

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
