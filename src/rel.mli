(*---------------------------------------------------------------------------
   Copyright (c) 2017 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Relational programming for OCaml.

    [Rel] is an embedded typed relational programming language.

    {b References.}
    {ul
    {- Jason Hemann and Daniel P. Friedman.
    {{:http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf}
    {e microKanren: A Minimal
    Functional Core for Relational Programming}}. In (Scheme '13), 2013.}}

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Terms} *)

type 'a term
(** The type for terms denoting values of type ['a]. *)

(** {2 Constants} *)

val const : ?eq:('a -> 'a -> bool) -> 'a -> 'a term
(** [const ~eq v] is a term for the constant [v]. During unification
    the constant is tested for equality with [eq] (defaults to
    {!Pervasives.( = )}).

    {b Note.} Two constants with physically different [eq] function
    never unify. *)

val unit : unit term
(** [unit] is [const ()]. *)

val bool : bool -> bool term
(** [bool b] is [const b]. *)

val int : int -> int term
(** [int i] is [const i] *)

(* val pair : 'a term -> 'b term -> ('a * 'b) term *)

(** {2 Finite tuples} *)

type ('k, 'a) tupler
val prod : ('k, 'b) tupler -> ('a term -> 'k, 'b) tupler
val stop : ('a term, 'a) tupler
val tuple : ('a, 'b) tupler -> 'a

(** {1 Goals} *)

type goal
(** The type for goals. A goal either {e succeeds} or {e fails}. *)

val fail : goal
(** [fail] is a goal that always fails. *)

val succeed : goal
(** [succeed] is a goal that always succeeds. *)

val ( = ) : 'a term -> 'a term -> goal
(** [t0 = t1] is a goal that succeeds iff there is a unifier for [t0] and
    [t1]. The unifier becomes part of the state. *)

val ( || ) : goal -> goal -> goal
(** [g0 || g1] is a goal that succeeds if either [g0] or [g1] succeeds. *)

val ( && ) : goal -> goal -> goal
(** [g0 && g1] is a goal succeeds if both [g0] and [g1] succeed. *)

val fresh : ('a term -> goal) -> goal
(** [fresh f] is the [f v] with [v] a fresh logical variable. *)

val success : goal -> bool
(** [success g] is [true] iff [g] succeeds on the empty state. *)

(** {1 Sequences of values} *)

type 'a seq
val next : 'c seq -> ('c * 'c seq) option
val all : 'c seq -> 'c list

(** {1 Reifying goals} *)

type ('a, 'b) reify
val reify : 'a -> 'b -> ('a, 'b) reify
val var : ('a term -> 'b, 'a -> 'c) reify -> ('b, 'c) reify
val find : (goal, 'c) reify -> ('c seq, [`Undefined]) result
val get : (goal, 'c) reify -> 'c seq

(*---------------------------------------------------------------------------
   Copyright (c) 2017 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
