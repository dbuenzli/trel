(*---------------------------------------------------------------------------
   Copyright (c) 2017 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Relational programming for OCaml

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Terms} *)

type 'a term

val const : ?eq:('a -> 'a -> bool) -> 'a -> 'a term

val unit : unit -> unit term
val bool : bool -> bool term
val int : int -> int term
val pair : 'a term -> 'b term -> ('a * 'b) term

(** {1 Goals} *)

type goal
val fail : goal
val succeed : goal
val ( = ) : 'a term -> 'a term -> goal
val ( || ) : goal -> goal -> goal
val ( && ) : goal -> goal -> goal
val fresh : ('a term -> goal) -> goal
val success : goal -> bool

(** {1 Sequences of values} *)

type 'a seq
val next : 'c seq -> ('c * 'c seq) option
val all : 'c seq -> 'c list

(** {1 Finding goals} *)

type ('a, 'b) find
val lift : 'a -> 'b -> ('a, 'b) find
val var : ('a term -> 'b, 'a -> 'c) find -> ('b, 'c) find
val find : (goal, 'c) find -> ('c seq, [`Undefined]) result
val get : (goal, 'c) find -> 'c seq

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
