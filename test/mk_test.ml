(*---------------------------------------------------------------------------
   Copyright (c) 2017 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let assert_vals ?limit q vals =
  let q = Mk.(query @@ reifier q value_get) in
  assert (Mk.(seq_to_list ?limit @@ run q) = vals)

let assert_2vals ?limit q vals =
  let reify x y = Mk.(value_get x, value_get y) in
  let q = Mk.(query @@ query @@ reifier q reify) in
  assert (Mk.(seq_to_list ?limit @@ run q) = vals)

let listo d dl =
  let empty = Mk.(const dl []) in
  let cons x xs =
    Mk.(pure (fun x xs -> x :: xs) |> app d x |> app dl xs |> ret dl)
  in
  let list l = List.fold_right (fun x xs -> cons (Mk.const d x) xs) l empty in
  let rec appendo l0 l1 l =
    let open Mk in
    (l0 = empty && l1 = l) ||
    (fresh @@ fun x -> fresh @@ fun xs -> fresh @@ fun tl ->
     cons x xs = l0 &&
     cons x tl = l  &&
     delay @@ lazy (appendo xs l1 tl))
  in
  let rec revo l r =
    let open Mk in
    (l = empty && r = empty) ||
    (fresh @@ fun x -> fresh @@ fun xs -> fresh @@ fun rt ->
     cons x xs = l &&
     appendo rt (cons x empty) r &&
     delay @@ lazy (revo xs rt))
  in
  empty, cons, list, appendo, revo

let dint = Mk.dom ~equal:(( = ) : int -> int -> bool)
let int = Mk.const dint

let dilist = Mk.dom ~equal:( = )
let iempty, icons, ilist, iappendo, irevo = listo dint dilist

let test_match () =
  let m4tch x xs = Mk.(icons x xs = ilist [1;2;3]) in
  assert_2vals m4tch [(1, [2;3])];
  ()

let test_appendo () =
  assert_vals (fun l -> iappendo (ilist [1;2]) (ilist [3;4]) l) [[1;2;3;4]];
  assert_vals (fun l -> iappendo l (ilist [3;4]) (ilist [1;2;3;4])) [[1;2]];
  assert_vals (fun l -> iappendo (ilist [1;2]) l (ilist [1;2;3;4])) [[3;4]];
  ()

let test_pre_suf () =
  let pre l pre = Mk.(fresh @@ fun suf -> iappendo pre suf l) in
  let suf l suf = Mk.(fresh @@ fun pre -> iappendo pre suf l) in
  let pre_suf l pre suf = iappendo pre suf l in
  let l = ilist [1;2;3;4] in
  assert_vals (pre l) [[]; [1]; [1;2]; [1;2;3]; [1;2;3;4]];
  assert_vals (suf l) [[1;2;3;4]; [2;3;4]; [3;4]; [4]; []];
  assert_2vals (pre_suf l)
    [([], [1;2;3;4]);
     ([1], [2;3;4]);
     ([1;2], [3;4]);
     ([1;2;3], [4]);
     ([1;2;3;4], [])];
  ()

let test_revo () =
  assert_vals ~limit:1 (fun r -> irevo (ilist [1;2;3]) r) [[3;2;1]];
  assert_vals ~limit:1 (fun r -> irevo r (ilist [1;2;3])) [[3;2;1]];
  ()

let test () =
  test_match ();
  test_appendo ();
  test_pre_suf ();
  test_revo ();
  print_endline "All Mk tests succeeded!";
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
