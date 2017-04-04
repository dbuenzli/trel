(*---------------------------------------------------------------------------
   Copyright (c) 2017 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Relational lists *)

(** {1 Lists} *)

(** The type for list elements *)
module type EL = sig
  type t
  val dom : t Rel.dom
end

(** [Make_el (V)] are list elements from the domainable module [V]. *)
module Make_el (V : Rel.Dom.V) : EL with type t = V.t

(** [Make (E)] is a module for relational lists with elements of type [E]. *)
module Make (E : EL) : sig

  (** {1 Relational lists} *)

  type t = E.t list Rel.term
  (** The type for relational lists. *)

  type e = E.t Rel.term
  (** The type for relational list elements. *)

  val dom : E.t list Rel.dom

  (** {1 Term constructors} *)

  val empty : t
  (** [empty] is the empty list. *)

  val cons : e -> t -> t
  (** [cons x xs] is the list [x :: xs]. *)

  val v : E.t list -> t
  (** [v l] is [l] as a relational list. *)

  (** {1 Relational operations} *)

  val is_empty : t -> Rel.goal
  (** [is_empty l] is [Rel.(empty = l)]. *)

  val hd : t -> e -> Rel.goal
  (** [hd l x] succeeds if [x] is the head of [l]. *)

  val tl : t -> t -> Rel.goal
  (** [tl l xs] succeeds if [xs] is the tail of [l]. *)

  val append : t -> t -> t -> Rel.goal
  (** [append l0 l1 l] succeeds if [l1] appended to [l0] is [l]. *)

  val rev : t -> t -> Rel.goal
  (** [rev l r] succeeds if [r] is the reverse list of [l]. *)

  val mem : e -> t -> Rel.goal
  (** [mem e l] succeeds if [e] is a member of [l]. *)
end

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
