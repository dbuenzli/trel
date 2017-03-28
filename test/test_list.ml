(*---------------------------------------------------------------------------
   Copyright (c) 2017 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let assert_vals ?limit q vals =
  let q = Rel.(query @@ reifier q Value.get) in
  assert (Rel.(Seq.to_list ?limit @@ run q) = vals)

let query ?limit q =
  let q = Rel.(query @@ reifier q Value.get) in
  Rel.(Seq.to_list ?limit @@ run q)


module L = Rel_list.Make (struct type t = int let dom = Rel.Dom.int end)

let test_base () =
  assert_vals (fun x -> L.(hd (v [1;2;3]) x)) [1];
  assert_vals (fun x -> L.(tl (v [1;2;3]) x)) [[2;3]];
  ()

let test () =
  test_base ();
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
