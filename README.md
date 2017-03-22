Rel — Relational programming for OCaml
-------------------------------------------------------------------------------
%%VERSION%%

Rel is a typed relational programming language embedded in OCaml. Its
term language is fully extensible and allows the client to inject and
project OCaml types and functions to/from it.

Rel is a typed and type-safe implementation of [microKanren][microKanren].

[microKanren]: http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf

Rel is distributed under the ISC license.

Homepage: http://erratique.ch/software/rel  

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
