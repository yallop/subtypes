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

module Eq : EQ =
struct
  type ('a, 'b) t = {P:CONTEXT} -> 'a P.t -> 'b P.t
  let refl {P:CONTEXT} x = x
  let lift {Q:CONTEXT} (f : (_,_) t) {R:CONTEXT} =
    let module M = struct type 'a t = 'a Q.t R.t end in f {M}
  let coerce (f: ('a,'b) t) = let module Id = struct type 'a t = 'a end in f {Id}
end

let symm : type a b. (a, b) Eq.t -> (b, a) Eq.t =
  fun (type a) (type b) (eq : (a, b) Eq.t) ->
    let module M = struct type 'a t = ('a,a) Eq.t end in
    (Eq.coerce (Eq.lift {M} eq) Eq.refl)
