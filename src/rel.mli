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

val string : string -> string term
(** [string s] is [const s]. *)

val pair : unit -> 'a term -> 'b term -> ('a * 'b) term
(** [pair ()] is a pair for two types of terms. *)

(** {2:func Functions} *)

type ('a, 'b) func
(** The type for lifting functions into the term language. ['b] is
    eventually a function from terms to terms whose application
    denotes application of the corresponding function. *)

val constf : 'a -> ('a term -> 'b, 'b) func
(** [constf f] is the function [f] as a term. *)

val arg :
  (('a -> 'b) term -> 'c, 'd) func -> (('b term -> 'c), 'a term -> 'd) func
(** [arg fc] constructs the function from terms to terms of the underyling
    lifted function, argument by argument. *)

val func : ('a -> 'a, 'b) func -> 'b
(** [func fc] is the function from terms to terms to lift the underlying
    function to the term language. This can be used to unify application
    of this function. *)

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

val delay : goal Lazy.t -> goal
(** [delay gazy] sees the lazy goal [gazy] as a goal. *)

(** {1 Streams of values} *)

type 'a stream
val next : 'c stream -> ('c * 'c stream) option
val all : 'c stream -> 'c list
val head : 'c stream -> 'c

(** {1 Reifying goals} *)

type ('a, 'b) reify
val reify : 'a -> 'b -> ('a, 'b) reify
val var : ('a term -> 'b, 'a -> 'c) reify -> ('b, 'c) reify
val find : (goal, 'c) reify -> ('c stream, [`Undefined]) result
val get : (goal, 'c) reify -> 'c stream

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
