(*---------------------------------------------------------------------------
   Copyright (c) 2017 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let assert_vals ?limit d q vals =
  let q = Mkv.(query d @@ reifier q value_get) in
  assert (Mkv.(seq_to_list ?limit @@ run q) = vals)

let assert_find_vals ?limit d q vals =
  let q = Mkv.(query d @@ reifier q value_find) in
  assert (Mkv.(seq_to_list ?limit @@ run q) = vals)

let assert_2vals ?limit d0 d1 q vals =
  let reify x y = Mkv.(value_get x, value_get y) in
  let q = Mkv.(query d1 @@ query d0 @@ reifier q reify) in
  assert (Mkv.(seq_to_list ?limit @@ run q) = vals)

let dint = Mkv.dom ~equal:(( = ) : int -> int -> bool)
let int = Mkv.const dint

let test_fair_disj () =
  let rec fivel x = Mkv.(x = int 5 || delay @@ lazy (fivel x)) in
  let rec fiver x = Mkv.(delay @@ lazy (fiver x) || x = int 5) in
  assert_vals ~limit:3 dint fivel [5;5;5];
  assert_vals ~limit:3 dint fiver [5;5;5];
  ()

let test_unfair_conj () =
  let rec faill x = Mkv.(fail && delay @@ lazy (faill x)) in
  assert_find_vals ~limit:3 dint faill [];
(*
  let rec failr x = Mkv.(delay @@ lazy (failr x) && fail) in
  assert_find_vals ~limit:3 dint failr []; *)
  ()

let listo d dl =
  let empty = Mkv.(const dl []) in
  let cons x xs =
    Mkv.(pure (fun x xs -> x :: xs) |> app x |> app xs |> ret dl)
  in
  let list l = List.fold_right (fun x xs -> cons (Mkv.const d x) xs) l empty in
  let rec appendo l0 l1 l =
    let open Mkv in
    (l0 = empty && l1 = l) ||
    (fresh d @@ fun x -> fresh dl @@ fun xs -> fresh dl @@ fun tl ->
     cons x xs = l0 &&
     cons x tl = l  &&
     delay @@ lazy (appendo xs l1 tl))
  in
  let rec revo l r =
    let open Mkv in
    (l = empty && r = empty) ||
    (fresh d @@ fun x -> fresh dl @@ fun xs -> fresh dl @@ fun rt ->
     cons x xs = l &&
     appendo rt (cons x empty) r &&
     delay @@ lazy (revo xs rt))
  in
  empty, cons, list, appendo, revo

let dilist = Mkv.dom ~equal:( = )
let iempty, icons, ilist, iappendo, irevo = listo dint dilist

let test_match () =
  let m4tch x xs = Mkv.(icons x xs = ilist [1;2;3]) in
  assert_2vals dint dilist m4tch [(1, [2;3])];
  ()

let test_appendo () =
  assert_vals dilist
    (fun l -> iappendo (ilist [1;2]) (ilist [3;4]) l) [[1;2;3;4]];
  assert_vals dilist
    (fun l -> iappendo l (ilist [3;4]) (ilist [1;2;3;4])) [[1;2]];
  assert_vals dilist
    (fun l -> iappendo (ilist [1;2]) l (ilist [1;2;3;4])) [[3;4]];
  ()

let test_pre_suf () =
  let pre l pre = Mkv.(fresh dilist @@ fun suf -> iappendo pre suf l) in
  let suf l suf = Mkv.(fresh dilist @@ fun pre -> iappendo pre suf l) in
  let pre_suf l pre suf = iappendo pre suf l in
  let l = ilist [1;2;3;4] in
  assert_vals dilist (pre l) [[]; [1]; [1;2]; [1;2;3]; [1;2;3;4]];
  assert_vals dilist (suf l) [[1;2;3;4]; [2;3;4]; [3;4]; [4]; []];
  assert_2vals dilist dilist (pre_suf l)
    [([], [1;2;3;4]);
     ([1], [2;3;4]);
     ([1;2], [3;4]);
     ([1;2;3], [4]);
     ([1;2;3;4], [])];
  ()

let test_revo () =
  assert_vals ~limit:1 dilist (fun r -> irevo (ilist [1;2;3]) r) [[3;2;1]];
  assert_vals ~limit:1 dilist (fun r -> irevo r (ilist [1;2;3])) [[3;2;1]];
  ()

let test () =
  test_fair_disj ();
  test_unfair_conj ();
  test_match ();
  test_appendo ();
  test_pre_suf ();
  test_revo ();
  print_endline "All Mkv tests succeeded!";
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
