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


(* Example *)
module PosList = struct type 'a t = 'a list end
module PosSnd = struct type 'a t = int * 'a end
let apply : type a b . (a, b) Sub.t -> (int * a) list -> (int * b) list =
  fun s ->
    coerce (lift {PosList} (lift {PosSnd} s))




(* This definition is an alternative to subtyping witnesses defined by contexts *)
module CtxSub = struct
  type (-'a, +'b) t = {P:POS} -> ('a P.t -> 'b P.t)
  let refl : 'a. ('a, 'a) t =
    fun {P:POS} x -> x
end

(* We can convert back and forth between both types of witness *)

let iso_1 : type a b . (a, b) Sub.t -> (a, b) CtxSub.t =
  fun s -> elim {CtxSub} s

let iso_2 : type a b . (a, b) CtxSub.t -> (a, b) Sub.t =
  fun s ->
  let module P = struct type 'b t = (a, 'b) Sub.t end in
  s {P} Sub.refl
