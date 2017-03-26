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

(** {1 Domains} *)

type 'a dom
(** The type for domains for values of type ['a]. *)

val dom :
  ?pp:(Format.formatter -> 'a -> unit) -> ?equal:('a -> 'a -> bool) -> unit ->
  'a dom
(** [dom ~pp ~eq] is a new domain using [equal] to test values for equality
    (defaults to {!Pervasives.( = )}) and [pp] to print them (defaults
    to a formatter that constantly prints ["<abstr>"]). *)

(** Domains. *)
module D : sig

  (** {1:dom Domains} *)

  type 'a t = 'a dom
  (** The type for domains, see {!type:dom}. *)

  val v :
    ?pp:(Format.formatter -> 'a -> unit) -> ?equal:('a -> 'a -> bool) ->
    unit -> 'a dom
  (** [v] is {!val:dom}. *)

  (** The type for modules that can be seen as domains. *)
  module type V = sig
    type t
    (** The type of the values of the domain. *)

    val equal : t -> t -> bool
    (** [equal v v'] is [true] iff [v] and [v'] are equal. *)

    val pp : Format.formatter -> t -> unit
    (** [pp ppf v] prints an unspecified representation of [v]
        on [ppf]. *)
  end

  val of_type : (module V with type t = 'a) -> 'a t
  (** [of_type m] is a domain from the module [m]. *)

  val with_pp : (Format.formatter -> 'a -> unit) -> 'a dom -> 'a dom
  (** [with_pp pp d] is domain [d] with pretty-printer [pp]. The
      resulting domain is {!equal} to [d]. *)

  val equal : 'a dom -> 'b dom -> bool
  (** [equal d0 d1] is [true] iff [d0] and [d1] are the same domain. *)

  (** {1:base Base type domains} *)

  val unit : unit dom
  (** [unit] is a domain for the [()] value. *)

  val bool : bool dom
  (** [bool] is a domain for [bool] values. *)

  val int : int dom
  (** [int] is a domain for [int] values. *)

  val float : float dom
  (** [float] is a domain for [float] values. *)

  val string : string dom
  (** [string] is a domain for [string] values. *)

  (** {1:poly Domains for polymorphic types} *)

  val pair : 'a dom -> 'b dom -> ('a * 'b) dom
  (** [pair fst snd] is a domain for pairs with first projection [fst]
      and second projection [snd]. *)

  val list : 'a dom -> 'a list dom
  (** [list el] is a domain for lists of type ['a]. *)
end

(** {1 Terms} *)

type 'a term
(** The type for terms denoting values of type ['a]. *)

(** {2 Constants} *)

val const : 'a dom -> 'a -> 'a term
(** [const dom v] is a term for the constant [v] in domain [dom].

    Two constants of the same type with {{!D.equal}non equal} domains
    never unify. *)

val unit : unit term
(** [unit] is [const D.unit ()]. *)

val bool : bool -> bool term
(** [bool b] is [const D.bool b]. *)

val int : int -> int term
(** [int i] is [const D.int i] *)

val float : float -> float term
(** [float f] is [const D.float f]. *)

val string : string -> string term
(** [string s] is [const D.string s]. *)

(** {2:fapp Function applications}

    Two function applications (constructor) {{!ret}returning} values in the
    same domain unify if each of their argument unify. *)

type 'a app
(** The type for function applications returning values of type ['a]. *)

val pure : 'a -> 'a app
(** [pure f] is the application that yields [f]. *)

val app : 'a dom -> 'a term -> ('a -> 'b) app -> 'b app
(** [app d t app] is the application of term [t] interpreted in domain [d]
    to the function of [app]. *)

val ret : 'b dom -> 'b app -> 'b term
(** [ret d app] is a term that interprets the application [app] in
    domain [d] and returns a term representing the function *)

(** {2:ptypes Polymorphic types} *)

val pair :
  'a dom -> 'b dom -> ('a * 'b) dom -> 'a term -> 'b term ->  ('a * 'b) term
(** [pair dom fst snd] is a pair for [fst] and [snd] in [dom]. *)

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
