(*---------------------------------------------------------------------------
   Copyright (c) 2017 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Eliza
   Most of the rules are from:
    Paradigms of Artificial Intelligence Programming. Peter Norvig. 1992.
    p. 172. See http://norvig.com/paip/eliza.lisp.
   which claim to include most of Weizenbaum's original rules. *)

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
  let m4tch l x y z = Fresh.v1 @@ fun c -> L.append c z l && L.append x y c in
  let _str l = L.v [l] in
  let cst s = L.empty, L.v [s], L.empty in
  Fresh.v2 @@ fun x y ->
  let d pat answs =
    m4tch input x (L.v pat) y &&
    (let add_answ acc (x, y, z) = acc || m4tch answ x y z in
     List.fold_left add_answ fail answs)
  in
  List.fold_left ( || ) fail [
    d ["hello"] [ cst "How do you do ? Please state your problem."];
(*
    d ["computer"] [
      (cst "Do computers worry you ?");
      (cst "What do you think about machines ?");
      (cst "Why do you mention computers ?");
      (cst "What do you think machines have to do with your problem ?")];
    d ["name"] [ cst "I am not interested in names." ];
    d ["sorry"] [
      (cst "Please don't apologize.");
      (cst "Apologies are not necessary.");
      (cst "What feelings do you have when you apologize") ];
    d ["I"; "remember"] [
      (str "Do you often think of", y, str "?");
      (str "Does thinking of", y, str "bring anything else to mind ?");
      (cst "What else do you remember ?");
      (str "Why do you recall", y, str "right now ?");
      (str "What in the present situation reminds you of", y, str "?");
      (str "What is the connection between me and", y, str "?") ];
    d ["do"; "you"; "remember"] [
      (str "Did you think I would forget", y, str "?");
      (str "Why do you think I should recall", y, str "now ?");
      (str "What about", y, str "?");
      (str "You mentioned", y, str ".") ];
    d ["if"] [
      (str "Do you really think its likely that", y, str "?");
      (str "Do you wish that", y, str "?");
      (str "What do you think about", y, str "?");
      (str "Really--if", y, str "?") ];
    d ["I"; "dreamt"] [
      (cst "What does this dream suggest to you ?");
      (cst "Do you dream often ?");
      (cst "What persons appear in your dreams ?");
      (cst "Don't you believe that dream has to do with your problem ?")];
    d ["my"; "mother"] [
      (str "Who else in your family", y, str "?");
      (cst "Tell me more about your family.") ];
    d ["my"; "father"] [
      (cst "Your father.");
      (cst "Does he influence you strongly ?");
      (cst "What else comes to mind when you think of your father ?")];
    d ["I"; "want"] [
      (str "What would it mean if you got", y, str "?");
      (str "Why do you want", y, str "?");
      (str "Suppose you got", y, str "soon.")];
    d ["I"; "am"; "glad"] [
      (str "How have I helped you to be", y, str "?");
      (cst "What makes you happy just now ?");
      (str "Can you explain why you are suddenly", y, str "?") ];
    d ["I"; "am"; "sad"] [
      (cst "I am sorry to hear you are depressed.");
      (cst "I'm sure it's not pleasant to be sad.") ];
(*
    d ["are"; "like"] [
      (str "What resemblance do you see between", x, str "and", y) ]
*)
    d ["is"; "like"] [
   (*   (str "In what way is it that", x, str "is like", y); *)
      (cst "What resemblance do you see ?");
      (cst "Could there really be some connection?");
      (cst "How?") ];
    d ["alike"] [
      (cst  "In what way ?");
      (cst "What similarities are there ?");
    ];
    d ["same"] [ (cst "What other connections do you see ?") ];
    d ["I"; "was"] [
      (cst "Where you really ?");
      (str "Perhaps I already knew you were", y, str ".");
      (str "Why do you tell me you were", y, str "now ?") ];
    d ["was"; "I"] [
      (str "What if you were", y, str "?");
      (str "Do you think you were", y, str "?");
      (str "What would it mean if you were", y, str "?") ];
    d ["I"; "am"] [
      (str "In what way are you", y, str "?");
      (str "Do you want to be", y, str "?") ];
    d ["am"; "I"] [
      (str "Do you believe you are", y, str "?");
      (str "Would you want to be", y, str "?");
      (str "You wish I would tell you you are", y, str "?");
      (str "What would it mean if you were", y, str "?") ];
    d [ "am" ] [
      (cst "Why do you say \"AM\" ?");
      (cst "I don't understand that.") ];
    d ["are"; "you" ] [
      (str "Why are you interested in whether I am", y, str "or not ?");
      (str "Would you prefer if I weren't", y, str "?");
      (str "Perhaps I am", y, str "in your fantasies.") ];
    d ["you"; "are"] [
      (str "What makes you think I am", y, str "?") ];
    d ["because"] [
      (cst "Is that the real reason ?");
      (cst "What other reasons might there be ?");
      (cst "Does that reason seem to explain anything else ?") ];
    d ["were"; "you"] [
      (str "Perhaps I was", y, str "?");
      (cst "What do you think ?");
      (str "What if I had been", y, str "?") ];
    d ["I"; "can't" ] [
      (str "Maybe you could", y, str "now");
      (str "What if you could", y, str "?") ];
    d ["I"; "feel"] [ (str "Do you often feel", y, str "?") ];
    d ["I"; "felt"] [ (cst "What other feelings do you have ?") ];
(*
   (((?* ?x) I (?* ?y) you (?* ?z))
    (Perhaps in your fantasy we ?y each other))
*)
    d ["why"; "don't"; "you"] [
      (str "Should you", y, str "yourself ?");
      (str "Do you believe I don't", y, str "?");
      (str "Perhaps I will", y, str "in good time") ];
    d [ "yes" ] [
      (cst "You seem quite positive.");
      (cst "You are sure.");
      (cst "I understand.") ];
    d ["no"] [
      (cst "Why not ?");
      (cst "You are being a bit negative.");
      (cst "Are you saying \"NO\" just to be negative ?") ];
   d ["someone"] [ (cst "Can you be more specific ?") ];
   d ["everyone"] [
     (cst "Surely not everyone");
     (cst "Can you think of anyone in particular ?");
     (cst "Who for example ?");
     (cst "You are thinking of a special person.")];
    d ["always"] [
      (cst "Can you think of a specific example") ;
      (cst "When ?");
      (cst "What incident are you thinking of ?");
      (cst "Really-- always ?"); ];
    d ["what"] [
      (cst "Why do you ask ?");
      (cst "Does that question interest you ?");
      (cst "What is it you really want to know ?");
      (cst "What do you think?");
      (cst "What comes to your mind when you ask that ?") ];
    d ["perhaps"] [ (cst "You do not seem quite certain.") ];
    d ["are"] [
      (str "Did you think they might not be", y, str "?");
      (str "Posysibly they are", y, str "?")];
    d ["obj"] [
      (cst "Obj is not part of the language.");
      (cst "Xavier is not going to be happy about this."); ] *)]

let answer input =
  let input = L.v (Tok.of_string input) in
  let answs = match Trel.Run.get1 (eliza input) with
  | [] -> [["Very intereysting."]; ["I am not sure I understand you fully"];
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
