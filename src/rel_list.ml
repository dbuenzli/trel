(*---------------------------------------------------------------------------
   Copyright (c) 2017 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

module type ELT = sig
  type t
  val dom : t Rel.dom
end

module Make_elt (V : Rel.Dom.V) = struct
  type t = V.t
  let dom = Rel.Dom.of_type (module V)
end

module Make (Elt : ELT) = struct

  type t = Elt.t list Rel.term
  type elt = Elt.t Rel.term
  let dom = Rel.Dom.list Elt.dom

  (* Term constructors *)

  let empty = Rel.const dom []
  let cons x xs = Rel.(pure List.cons |> app Elt.dom x |> app dom xs |> ret dom)

  let rec v = function (* not T.R. *)
  | [] -> empty
  | x :: xs -> cons (Rel.const Elt.dom x) (v xs)

  (* Relational operations *)

  let is_empty l = Rel.(l = empty)
  let hd l x = Rel.(fresh @@ fun xs -> cons x xs = l)
  let tl l xs = Rel.(fresh @@ fun x -> cons x xs = l)
  let rec mem x l =
    let open Rel in
    hd l x || Fresh.v1 @@ fun t -> tl l t && delay (lazy (mem x t))

  let rec append l0 l1 l =
    let open Rel in
    (l0 = empty && l1 = l) ||
    (Fresh.v3 @@ fun x xs ltl ->
     cons x xs = l0 &&
     cons x ltl = l  &&
     delay @@ lazy (append xs l1 ltl))

  let rec rev l r =
    let open Rel in
    (l = empty && r = empty) ||
    (Fresh.v3 @@ fun x xs rt ->
     cons x xs = l &&
     append rt (cons x empty) r &&
     delay @@ lazy (rev xs rt))
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
