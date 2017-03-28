Rel — Relational programming for OCaml
-------------------------------------------------------------------------------
%%VERSION%%

Rel is a typed relational programming language embedded in OCaml. Its
term language is fully extensible and allows the client to inject and
project OCaml types and functions to/from it.

Rel is a typed and type-safe implementation of [μKanren][microKanren].

[microKanren]: http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf

Rel is distributed under the ISC license.

Homepage: http://erratique.ch/software/rel

## Minimal typed μKanren implementation

Consult [`mk.mli`](test/mk.mli) and [`test/mk.ml`](test/mk.ml) and its
[tests](test/mk_test.ml) for an absolute minimal typed and type-safe
μKanren in the spirit of the original one.

The implementation shows how to define an arbitrarily extensible typed
term language and an API for typed reification. This is the core on
which `Rel` adds a bit of convenience.

Other OCaml implementations of {mini,μ}Kanren are listed on the 
[miniKanren website](http://minikanren.org/).

## Installation

Rel can be installed with `opam`:

    opam install rel

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation and API reference is generated from the source
interfaces. It can be consulted [online][doc] or via `odig doc
rel`.

[doc]: http://erratique.ch/software/rel/doc

## Sample programs

If you installed Rel with `opam` sample programs are located in
the directory `opam var rel:doc`.

In the distribution sample programs and tests are located in the
[`test`](test) directory. They can be built and run
with:

    topkg build --tests true && topkg test 
