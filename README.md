## Subtypes: first-class subtypes for OCaml

This package provides first-class subtypes for OCaml, analogous to Generalized Algebraic Data Types (GADTs).

GADTS support first-class type equality witnesses, so that a value [`(a, b) Type.eq`](https://github.com/ocaml/ocaml/blob/2a1cdfaa9d1d0b545ce9dc7ba7bf36232e59a56e/stdlib/type.mli#L22) is a run-time warrant that `a` and `b` are equal.

Analogously, this package supports first-class subtyping witnesses, so that a value `(a, b) Subtype.t` is a run-time warrant that `a` is a subtype of `b`.

The following paper describes the implementation of first-class subtypes, along with a number of motivating examples, including covariant lazy values, iterations over arrays of elements of distinct types, and proofs of variance:

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;First-Class Subtypes ([pdf][paper])  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Jeremy Yallop and Stephen Dolan  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;ML/OCaml 2017

### Installation

The implementation depends on the experimental [modular implicits][modular-implicits] extension to OCaml.  The modified compiler and this library can be installed as follows:

```
opam switch 4.02.1+modular-implicits
opam pin add subtypes https://github.com/yallop/subtypes.git
```

### Examples

The [tests][tests] directory has a number of examples.

[paper]: https://www.cl.cam.ac.uk/~jdy22/papers/first-class-subtypes-draft.pdf
[modular-implicits]: https://www.cl.cam.ac.uk/~jdy22/papers/modular-implicits.pdf
[tests]: https://github.com/yallop/subtypes/tree/master/tests
