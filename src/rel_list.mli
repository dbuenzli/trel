(*---------------------------------------------------------------------------
   Copyright (c) 2017 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Relational lists *)

(** {1 Lists} *)

(** The type for list elements *)
module type ELT = sig
  type t
  (** The type of list elements. *)

  val dom : t Rel.dom
  (** The domain of list elements. *)
end

(** [Make_elt (V)] are list elements from the domainable module [V]. *)
module Make_elt (V : Rel.Dom.V) : ELT with type t = V.t

(** [Make (E)] is a module for relational lists with elements of type [E]. *)
module Make (Elt : ELT) : sig

  (** {1 Relational lists} *)

  type t = Elt.t list Rel.term
  (** The type for relational lists. *)

  type elt = Elt.t Rel.term
  (** The type for relational list elements. *)

  val dom : Elt.t list Rel.dom

  (** {1 Term constructors} *)

  val empty : t
  (** [empty] is the empty list. *)

  val cons : elt -> t -> t
  (** [cons x xs] is the list [x :: xs]. *)

  val v : Elt.t list -> t
  (** [v l] is [l] as a relational list. *)

  (** {1 Goals} *)

  val is_empty : t -> Rel.goal
  (** [is_empty l] is [Rel.(empty = l)]. *)

  val mem : elt -> t -> Rel.goal
  (** [mem e l] succeeds if [e] is a member of [l]. *)

  val hd : t -> elt -> Rel.goal
  (** [hd l x] succeeds if [x] is the head of [l]. *)

  val tl : t -> t -> Rel.goal
  (** [tl l xs] succeeds if [xs] is the tail of [l]. *)

  val append : t -> t -> t -> Rel.goal
  (** [append l0 l1 l] succeeds if [l1] appended to [l0] is [l]. *)

  val rev : t -> t -> Rel.goal
  (** [rev l r] succeeds if [r] is the reverse list of [l]. *)
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
