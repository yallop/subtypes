open Subtypes

type (-'a,+'b) sub = ('a, 'b) Sub.t
let refl = Sub.refl
let coerce sub x = Sub.(>:) x sub

(* Some arrays *)

let a = [| object method x = 42 method name = "Foo" end |]
let b = [| object method y : [`Bwuh] = `Bwuh method name = "Bar" end |]

let print_name x = Printf.printf "name is %s\n" (x#name)


(* We can iterate over these arrays, printing names *)
let () =
  Array.iter print_name a;
  Array.iter print_name b

(* We'd like to combine those two calls:

   let arrays = [a; b]
   let () =
     List.iter (Array.iter print_name) arrays

   But we get type errors on [a; b].
   Subtype coercions are no help - there's no type that a and b
   can both be coerced to, because arrays are invariant. *)


(* First attempt: row polymorphism via GADTs *)

type array_of_named =
    Array_of_named : <name : string; ..> array ->
    array_of_named

let arrays = [ Array_of_named a; Array_of_named b ]

let () = List.iter (fun (Array_of_named x) -> Array.iter print_name x) arrays

(* This works, but only for <name: string>. We have to redo this for
   every set of field names and types that we care about.

   What we really want is to treat arrays as covariant here, instead
   of invariant, so that we can pretend a and b are <name : string>
   arrays. Since (Array.iter print_name) does not mutate its argument,
   this would be sound.

   We can do this using existential types and subtyping witnesses to
   give us bounded polymorphism. *)

type +'a array' = Array : 'x array * ('x, 'a) sub -> 'a array'
let of_array x = Array (x, refl)

(* We can't use Array.iter directly on an array', because the types
   don't exactly match. Instead, we apply the subtyping witness *)

let array'_iter f (Array (a, sub)) =
  Array.iter (fun s -> f (coerce sub s)) a


(* Now, we can write (more or less) what we originally wanted, using
   no infrastructure that's specific to <name : string> *)
let arrays = [
  (of_array a :> <name : string> array');
  (of_array b :> <name : string> array') ]
let () =
  List.iter (array'_iter print_name) arrays
