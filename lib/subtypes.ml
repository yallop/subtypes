(* This requires 4.02.0+modular-implicits or similar *)

module type POS = sig type +'a t end
module type NEG = sig type -'a t end
module Id = struct type 'a t = 'a end
module ComposePP(F:POS)(G:POS) = struct type 'a t = 'a F.t G.t end
module ComposePN(F:POS)(G:NEG) = struct type 'a t = 'a F.t G.t end

(*
 * Subtyping witnesses are the initial algebra over the following signature:
 *)
module type DIAG = sig
  type (-'a, +'b) t
  val refl : ('a, 'a) t
end

module Diag = struct
  type ('a, 'b) t = {D : DIAG} -> ('a, 'b) D.t

  let refl {D:DIAG} = D.refl

  let elim {D:DIAG} (f : ('a, 'b) t) = f {D}
end

module Function = struct
  type ('a, 'b) t = 'a -> 'b

  let refl x = x
end

(* We can lift a DIAG-algebra via any positive context *)
module LiftDiag (P : POS) (D : DIAG) = struct
  type ('a, 'b) t = ('a P.t, 'b P.t) D.t
  let refl = D.refl
end

(* the basic subtyping interface *)
module type SUB =
sig
  include DIAG
  val lift : {P:POS} -> ('a, 'b) t -> ('a P.t, 'b P.t) t
  val (>:) : 'a -> ('a, 'b) t -> 'b
end

module Sub = struct
  include Diag

  (* Initiality means we can map Sub to Function, giving subtyping coercions *)
  let (>:) x (sub : _ t) = sub {Function} x

  let lift {P : POS} (t : _ t) = t {LiftDiag (P) (Diag)}
end

(* We can convert between any two implementations of SUB. *)
let sub_of_sub (type a) {B:SUB} {A:SUB} (a : _ A.t) =
  let module P = struct type 'a t = (a, 'a) B.t end in
  A.(>:) B.refl (A.lift {P} a) 

module PosSub : SUB with type ('a, 'b) t = {P:POS} -> ('a P.t -> 'b P.t) =
struct
  type ('a, 'b) t = {P:POS} -> ('a P.t -> 'b P.t)

  let refl {P:POS} x = x

  let lift {P:POS} (s : _ t) {Q:POS} x = s {ComposePP(P)(Q)} x

  let (>:) x (f : _ t) = f {Id} x
end

module NegSub =
struct
  type ('a, 'b) t = {N:NEG} -> ('b N.t -> 'a N.t)

  let refl {N:NEG} x = x

  let lift {P:POS} (s : _ t) {Q:NEG} x = s {ComposePN(P)(Q)} x

  let id x = x

  let (>:) (type b) x (f : (_, b) t) =
    let module M = struct type -'a t = 'a -> b end in
    f {M} id x
end

let refl {S:SUB} = S.refl

let liftP {S:SUB} = S.lift

let (>:) {S:SUB} = S.(>:)

let trans : type a b c. {S:SUB} -> (a, b) S.t -> (b, c) S.t -> (a, c) S.t =
  fun {S:SUB} x y ->
  let module M = struct type +'c t = (a,'c) S.t end in
  S.(>:) x (S.lift {M} y)

let liftN : type a b. {S:SUB} -> {N:NEG} -> (a, b) S.t -> (b N.t, a N.t) S.t =
  fun {S:SUB} {N:NEG} x ->
  let module M = struct type +'b t = ('b N.t, a N.t) S.t end in
  S.(>:) S.refl (S.lift {M} x)
