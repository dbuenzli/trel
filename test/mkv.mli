(*---------------------------------------------------------------------------
   Copyright (c) 2017 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Straightforward minimal typed implementation of μKanren with
   domain specification on variables and return values. It seems
   much more inconvenient to have to type fresh and query variables. *)

type 'a dom
val dom : equal:('a -> 'a -> bool) -> 'a dom

type 'a term
type 'a ret

val const : 'a dom -> 'a -> 'a term
val pure : 'a -> 'a ret
val app : 'a term -> ('a -> 'b) ret -> 'b ret
val ret : 'a dom -> 'a ret -> 'a term

(* Goals *)

type goal

val fail : goal
val succeed : goal
val ( = ) : 'a term -> 'a term -> goal
val fresh : 'a dom -> ('a term -> goal) -> goal
val ( || ) : goal -> goal -> goal
val ( && ) : goal -> goal -> goal
val delay : goal Lazy.t -> goal

(* Reification *)

type 'a seq
val seq_to_list : ?limit:int -> 'a seq -> 'a list

type 'a value
val value_find : 'a value -> 'a option
val value_get : 'a value -> 'a

type ('q, 'r) reifier

val reifier : 'q -> 'r -> ('q, 'r) reifier
val query :
  'a dom -> ('a term -> 'q, 'a value -> 'r) reifier -> ('q, 'r) reifier

val run : (goal, 'a) reifier -> 'a seq

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
