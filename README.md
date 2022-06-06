Trel — Relational programming for OCaml
-------------------------------------------------------------------------------
%%VERSION%%

Trel is a typed relational programming language embedded in OCaml. Its
term language is arbitrarily extensible allowing clients to inject and
project OCaml values and functions for seamless interaction between
Trel and regular OCaml programs.

Trel is a typed and type-safe implementation of [μKanren][microKanren]
distributed under the ISC license. It depends on [fmt][fmt].

[microKanren]: http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf
[fmt]: http:/erratique.ch/software/fmt

Homepage: http://erratique.ch/software/trel

## Minimal typed μKanren implementation

Consult [`mk.mli`](test/mk.mli) and [`mk.ml`](test/mk.ml) and its
[tests](test/mk_test.ml) for an absolute minimal typed and type-safe
μKanren in the spirit of the original one.

The implementation shows how to define an arbitrarily extensible typed
term language and a simple API for typed reification of states. This
is the core on which `Trel` adds a bit of convenience.

Other OCaml implementations of {mini,μ}Kanren are listed on the 
[miniKanren website](http://minikanren.org/).

## Installation

Trel can be installed with `opam`:

    opam install trel

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation and API reference is generated from the source
interfaces. It can be consulted [online][doc] or via `odig doc
trel`.

[doc]: http://erratique.ch/software/trel/doc

## Sample programs

If you installed Trel with `opam` sample programs are located in
the directory `opam var trel:doc`.

In the distribution sample programs and tests are located in the
[`test`](test) directory. They can be built and run
with:

    topkg build --tests true && topkg test 
