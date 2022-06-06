(*---------------------------------------------------------------------------
   Copyright (c) 2017 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Eliza *)

open Astring

let random_elt = Random.self_init ();
fun l ->
  let a = Array.of_list l in
  let len = Array.length a in
  a.(Random.int len)

module Tok = struct
  type t = string
  let equal s0 s1 = String.(equal (Ascii.lowercase s0) (Ascii.lowercase s1))
  let pp = Format.pp_print_string
  let of_string s = String.fields ~empty:false s
end

module L = Trel_list.Make (Trel_list.Make_elt (Tok))

let eliza input answ =
  let open Trel in
  let m4tch l x y z = Fresh.v1 @@ fun t -> L.append x y t && L.append t z l in
  let lit l = L.v l in
  let str s = L.v [s] in
  let nil = L.empty in
  Fresh.v2 @@ fun x y ->
  (m4tch input x (lit ["Hello"]) y &&
   answ = str "How do you do ? Please state your problem.") ||
  (m4tch input x (lit ["I"; "want"]) y &&
   (m4tch answ (str "What would it mean if you got") y (str "?") ||
    m4tch answ (str "Why do you want") y (str "?") ||
    m4tch answ (str "Suppose you got") y (str "soon."))) ||
  (m4tch input x (lit ["if"]) y &&
   (m4tch answ (str "Do you really think its likely that") y (str "?") ||
    m4tch answ (str "Do you wish that") y (str "?") ||
    m4tch answ (str "What do you think about") y (str "?") ||
    m4tch answ (str "Really-- if") y (str "?"))) ||
  (m4tch input x (lit ["no"]) y &&
   (answ = str "Why not ?" ||
    answ = str "You are being a bit negative" ||
    answ = str "Are you saying \"NO\" just to be negative ?")) ||
  (m4tch input x (lit ["I"; "was"]) y &&
   (answ = str "Where you really ?" ||
    m4tch answ (str "Perhaps I already knew you were") y nil ||
    m4tch answ (str "Why do you tell me you were") y (str "now ?"))) ||
  (m4tch input x (lit ["I"; "feel"]) y &&
   m4tch answ (str "Do you often feel") y (str "?")) ||
  (m4tch input x (lit ["I"; "felt"]) y &&
   answ = str "What other feelings do you have ?") ||
  (m4tch input x (lit ["obj"]) y &&
   (answ = str "Obj is not part of the language" ||
    answ = str "Xavier is not going to be happy about this"))

let answer input =
  let input = L.v (Tok.of_string input) in
  let answs = match Trel.Run.get1 (eliza input) with
  | [] -> [["Very interesting."]; ["I am not sure I understand you fully"];
           ["What does that suggest to you ?"]; ["Please continue."];
           ["Go on"]; ["Do you feel strongly about discussing such things ?"]]
  | answs -> answs
  in
  String.concat ~sep:" " (random_elt answs)

let rec dialog () =
  print_string "eliza> "; flush stdout;
  match input_line stdin with
  | input -> print_endline (answer input); dialog ()
  | exception End_of_file -> print_endline ""; print_endline "Bye."

let () = dialog ()

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
