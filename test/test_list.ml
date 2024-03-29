(*---------------------------------------------------------------------------
   Copyright (c) 2017 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let assert_vals ?limit q vals = assert (Trel.Run.get1 ?limit q = vals)
let assert_fvals ?limit q vals = assert (Trel.Run.find1 ?limit q = vals)

module L = Trel_list.Make (struct type t = int let dom = Trel.Dom.int end)

let test_base () =
  assert_vals (fun x -> L.(hd (v [1;2;3]) x)) [1];
  assert_vals (fun x -> L.(tl (v [1;2;3]) x)) [[2;3]];
  ()

let test_mem () =
  assert_vals (fun u -> Trel.(L.(mem (int 1) (v [1;2;3])) && (u = unit))) [()];
  assert_vals (fun u -> Trel.(L.(mem (int 4) (v [1;2;3])) && (u = unit))) [];
  assert_vals (fun x -> Trel.(L.(mem x (v [1;2;3])))) [1;2;3];
  assert_vals (fun x -> Trel.(L.(mem x (v [])))) [];
  assert_vals
    (fun x -> Trel.(L.(mem (int 3) (cons (int 1) (cons x empty))))) [3];
  assert_fvals
    (fun x -> Trel.(L.(mem (int 3) (cons (int 3) (cons x empty)))))
    [None; Some 3];
  ()

let test_rev () =
  assert_vals (fun u -> Trel.(L.(rev (v [1;2;3]) (v [3;2;1])) && (u = unit)))
    [()];
  assert_vals ~limit:1 (fun l -> Trel.(L.(rev l (v [1;2;3])))) [[3;2;1]];
  assert_vals ~limit:1 (fun l -> Trel.(L.(rev (v [1;2;3]) l))) [[3;2;1]];
  ()

let test () =
  test_base ();
  test_mem ();
  test_rev ();
  print_endline "All list tests succeeded!";
  ()

let () = test ()


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
