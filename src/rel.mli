(*---------------------------------------------------------------------------
   Copyright (c) 2017 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Relational programming for OCaml.

    [Rel] is a typed, relational, programming language embedded in OCaml.

    See the {{!basics}basics}.

    {b References.}
    {ul
    {- Jason Hemann and Daniel P. Friedman.
    {{:http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf}
    {e microKanren: A Minimal
    Functional Core for Relational Programming}}. In (Scheme '13), 2013.}}

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1:doms Domains} *)

type 'a dom
(** The type for domains of values of type ['a]. *)

(** Domains. *)
module Dom : sig

  (** {1:dom Domains} *)

  type 'a t = 'a dom
  (** See {!type:dom}. *)

  val v :
    ?name:string -> ?pp:(Format.formatter -> 'a -> unit) ->
    ?equal:('a -> 'a -> bool) -> unit -> 'a dom
  (** [v ~name ~pp ~equal] is a new domain named [name] using [equal]
      to test values for equality and [pp] to print them. [name]
      defaults to ["unknown"], [equal] defaults to {!Pervasives.( = )}
      and [pp] to a formatter that constantly prints ["<abstr>"]). *)

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

  val with_dom :
    ?name:string -> ?pp:(Format.formatter -> 'a -> unit) -> 'a dom -> 'a dom
  (** [with_dom ~name ~pp d] is domain [d] with name [name] and
      pretty-printer [pp]. The resulting domain is {!equal} to [d]. *)

  val name : 'a dom -> string
  (** [name d] is [d]'s name. *)

  val pp_value : 'a dom -> Format.formatter -> 'a -> unit
  (** [pp_value d] is [d]'s value pretty-printer. *)

  val equal_value : 'a dom -> ('a -> 'a -> bool)
  (** [equal_value d] is [d]'s value equality function. *)

  val equal : 'a dom -> 'b dom -> bool
  (** [equal d0 d1] is [true] iff [d0] and [d1] are the same domain. *)

  val pp : Format.formatter -> 'a dom -> unit
  (** [pp ppf d] prints [d]'s {!name} on [ppf]. *)

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

(** {1:terms Terms} *)

type 'a term
(** The type for terms denoting values of type ['a]. *)

val pp_term : Format.formatter -> 'a term -> unit
(** [pp_term ppf t] prints an unspecified representation of [t] on [ppf]. *)

(** {2:csts Constants} *)

val const : 'a dom -> 'a -> 'a term
(** [const dom v] is a term for the constant [v] in domain [dom]. Two
    constants of the same type with {{!Dom.equal}different} domains
    never unify. *)

val unit : unit term
(** [unit] is [const Dom.unit ()]. *)

val bool : bool -> bool term
(** [bool b] is [const Dom.bool b]. *)

val int : int -> int term
(** [int i] is [const Dom.int i] *)

val float : float -> float term
(** [float f] is [const Dom.float f]. *)

val string : string -> string term
(** [string s] is [const Dom.string s]. *)

(** {2:fapp Function applications (constructors)}

    Two terms that represent function applications unify if
    the functions are physically equal, if each of their argument
    unifies and if the application's return values are in the
    same domain. *)

type 'a ret
(** The type for function applications returning values of type ['a]. *)

val pure : 'a -> 'a ret
(** [pure f] is the application that yields [f] itself. *)

val app : 'a dom -> 'a term -> ('a -> 'b) ret -> 'b ret
(** [app d t ret] is the application of term [t] interpreted in domain [d]
    to the function returned by [ret]. *)

val ret : 'b dom -> 'b ret -> 'b term
(** [ret d app] is a term that interprets the application [app] in
    domain [d] and returns a term representing the function application. *)

(** {2:ptypes Polymorphic types} *)

val pair :
  'a dom -> 'b dom -> ('a * 'b) dom -> 'a term -> 'b term ->  ('a * 'b) term
(** [pair dom fst snd] is a pair for [fst] and [snd] in [dom]. *)

(** {1 Goals} *)

type goal
(** The type for goals. In a given state a goal either {e succeeds} or
    {e fails}. *)

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

val delay : goal Lazy.t -> goal
(** [delay gazy] sees the lazy goal [gazy] as a goal. *)

val fresh : ('a term -> goal) -> goal
(** [fresh f] is the goal [f v] with [v] a fresh logical variable. *)

(** Multiple freshness introduction. *)
module Fresh : sig

  (** {1 Fresh} *)

  val v1 : ('a term -> goal) -> goal
  val v2 : ('a term -> 'b term -> goal) -> goal
  val v3 : ('a term -> 'b term -> 'c term -> goal) -> goal
  val v4 : ('a term -> 'b term -> 'c term -> 'd term -> goal) -> goal
  val v5 : ('a term -> 'b term -> 'c term -> 'd term -> 'e term -> goal) -> goal
  val v6 :
    ('a term -> 'b term -> 'c term -> 'd term -> 'e term -> 'f term -> goal) ->
    goal
end

(** {1 Reification} *)

type 'a seq
(** The type for (possibly infinite) sequences of values of type ['a]. *)

(** Sequences of values. *)
module Seq : sig

  (** {1 Sequences} *)

  type 'a t = 'a seq
  (** See {!seq}. *)

  val empty : 'a seq
  (** [empty] is the empty sequence. *)

  val is_empty : 'a seq -> bool
  (** [is_empty s] is true iff [s] is {!empty}. *)

  val head : 'a seq -> 'a option
  (** [head s] is [s]'s head (if any). *)

  val get_head : 'a seq -> 'a
  (** [get_head s] is like {!head} but @raise Invalid_argument if
      if [s] is {!empty}. *)

  val tail : 'a seq -> 'a seq
  (** [tail s] is [s]'s tail.

      @raise Invalid_argument if [s] is empty. *)

  val to_list : ?limit:int -> 'a seq -> 'a list
  (** [to_list ~limit s] is, at most, the first [limit] elements of [s].
      If [limit] is unspecified it is unbounded. *)
end

type 'a value
(** The type for representing the value of a variable of type ['a] in
    a given state. *)

(** Variable values. *)
module Value : sig

  type 'a t = 'a value
  (** See {!value}. *)

  val name : 'a value -> string
  (** [name v] is [v]'s name. See {!query}. *)

  val find : 'a value -> 'a option
  (** [find v] is [v]'s value, if any. *)

  val get : 'a value -> 'a
  (** [get v] is like {!find} but @raise Invalid_argument if [v] is
      undefined. *)

  val term : 'a value -> 'a term
  (** [term v] is [v]'s defining term. *)

  val pp : Format.formatter -> 'a value -> unit
  (** [pp ppf v] prints, if it exists, [v]'s value using the value's domain
      {{!Dom.pp_value}pretty-printer}. Otherwise it prints [v]'s {{!term}
      defining term}. *)

  (** {1 Multiple values as tuples} *)

  val get1 : 'a value -> 'a
  val get2 : 'a value -> 'b value -> 'a * 'b
  val get3 : 'a value -> 'b value -> 'c value -> 'a * 'b * 'c
  val get4 : 'a value -> 'b value -> 'c value -> 'd value -> 'a * 'b * 'c * 'd
  val get5 :
    'a value -> 'b value -> 'c value -> 'd value -> 'e value ->
    'a * 'b * 'c * 'd * 'e
  val get6 :
    'a value -> 'b value -> 'c value -> 'd value -> 'e value -> 'f value ->
    'a * 'b * 'c * 'd * 'e * 'f
end

type ('q, 'r) reifier
(** The type for reifiers. The type ['q] is the query to reify,
    the type ['r] is the state reifying function applied on each state. *)

val reifier : 'q -> 'r -> ('q, 'r) reifier
(** [reifier q f] reifies the query [q] with reifying function [f]. *)

val query :
  ?name:string -> ('a term -> 'q, 'a value -> 'r) reifier -> ('q, 'r) reifier
(** [query ~name r] introduces a logical query variable in [r]'s query and
    binds its value in the state reyifing function. [name] can be
    used to name the value. *)

(** Multiple query introduction. *)
module Query : sig

  (** {1 Query} *)

  val v1 :
    ?n0:string -> ('a term -> 'q, 'a value -> 'r) reifier -> ('q, 'r) reifier

  val v2 :
    ?n0:string -> ?n1:string ->
    ('a term -> 'b term -> 'q,
     'a value -> 'b value -> 'r) reifier -> ('q, 'r) reifier

  val v3 :
    ?n0:string -> ?n1:string -> ?n2:string ->
    ('a term -> 'b term -> 'c term -> 'q,
     'a value -> 'b value -> 'c value -> 'r) reifier -> ('q, 'r) reifier

  val v4 :
    ?n0:string -> ?n1:string -> ?n2:string -> ?n3:string ->
    ('a term -> 'b term -> 'c term -> 'd term -> 'q,
     'a value -> 'b value -> 'c value -> 'd value -> 'r) reifier ->
    ('q, 'r) reifier

  val v5 :
    ?n0:string -> ?n1:string -> ?n2:string -> ?n3:string -> ?n4:string ->
    ('a term -> 'b term -> 'c term -> 'd term -> 'e term -> 'q,
     'a value -> 'b value -> 'c value -> 'd value -> 'e value -> 'r) reifier ->
    ('q, 'r) reifier

  val v6 :
    ?n0:string -> ?n1:string -> ?n2:string -> ?n3:string -> ?n4:string ->
    ?n5:string ->
    ('a term -> 'b term -> 'c term -> 'd term -> 'c term -> 'f term -> 'q,
     'a value -> 'b value -> 'c value -> 'd value -> 'c value -> 'f value ->'r)
      reifier -> ('q, 'r) reifier
end


val run : (goal, 'r) reifier -> 'r Seq.t
(** [run r] is the sequence of states reified by [r]'s reifying
    function and obtained by running [r]'s query on the empty
    state. *)

val success : goal -> bool
(** [success g] is [true] iff [g] succeeds on the empty state. *)

(** {1:basics Basics}

    Fixme explain terms, goals, unification, recursion through delay and
    reification.

{[
let q x = Rel.(x = int 5)
let xs = Rel.(Seq.to_list @@ run @@ Query.v1 @@ reifier q Value.get1)
let () = assert (xs = [5])
]}

{[
let q x y = Rel.(y = int 6 && (x = y || x = int 5))
let xys = Rel.(Seq.to_list @@ run @@ Query.v2 @@ reifier q Value.get2)
let () = assert (xys = [(6, 6); (5, 6)])
]}

   {2:func Unifying function applications (constructors)}

   Represent lists in the term language:
{[
let intl = Rel.Dom.(list int)

let empty = Rel.const intl []
let cons x xs = Rel.(pure List.cons |> app Dom.int x |> app intl xs |> ret intl)

let rec ilist = function [] -> empty | i :: is -> cons (Rel.int i) (ilist is)
]}

  Unify lists:
{[
let l x xs = Rel.(cons x xs = ilist [1;2;3]) in
let ls = Rel.(Seq.to_list @@ run @@ Query.v2 @@ reifier q Value.get2)
let () = assert (ls = [(1;[2;3])]
]}

  Relational append. Express a predicate [appendo l0 l1 l] that asserts
  [l] is the concatenation of [l1] to [l0].
{[
let rec appendo l0 l1 l =
  let open Rel in
  (l0 = empty && l1 = l) ||
  (Fresh.v3 @@ fun x xs tl ->
   cons x xs = l0 &&
   cons x tl = l  &&
   delay @@ lazy (appendo xs l1 tl))
]}

  Find all lists that appended together give the list [[1;2;3]]:
{[
let q l0 l1 = appendo l0 l1 (ilist [1;2;3])
let l01s = Rel.(Seq.to_list @@ run @@ Query.v2 @@ reifier q Value.get2)
let () = assert (l01s =
    [([], [1;2;3]);
     ([1],  [2;3]);
     ([1;2],  [3]);
     ([1;2;3], [])])
]}
*)

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
