(*---------------------------------------------------------------------------
   Copyright (c) 2017 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let assert_vals ?limit q vals = assert (Trel.Run.get1 ?limit q = vals)
let assert_find_vals ?limit q vals = assert (Trel.Run.find1 ?limit q = vals)
let assert_2vals ?limit q vals = assert (Trel.Run.get2 ?limit q = vals)

let test_simple_unify () =
  assert_vals (fun q -> Trel.(q = int 5)) [5];
  assert_vals (fun q -> Trel.(q = int 5 || q = int 6)) [5;6];
  assert_2vals
    (fun a b -> Trel.(a = int 7 && (b = int 5 || b = int 6))) [(7, 5); (7, 6)];
  assert_vals
    (fun x -> Trel.(Fresh.v2 @@ fun y z -> x = y && y = z && z = int 3)) [3];
  ()

let test_pair () =
  let p2 = Trel.(pair Dom.int Dom.bool Dom.(pair int bool)) in
  let p p =
    let open Trel in
    Fresh.v2 @@ fun x y ->
    p = (p2 x y) && x = int 5 && y = bool true
  in
  assert_vals p [5, true];
  ()

let test_fapp () =
  let triple x y z = (x, y, z) in
  let t3 x y z =
    let open Trel in
    pure triple |> app Dom.int x |> app Dom.bool y |> app Dom.string z |>
    ret (Dom.v ())
  in
  let t t =
    let open Trel in
    Fresh.v3 @@ fun x y z ->
    t = t3 x y z && x = int 5 && y = bool true && z = string "bla"
  in
  assert_vals t [(5, true, "bla")];
  ()

let test_delay () =
  let rec fives x = Trel.(x = int 5 || delay (lazy (fives x))) in
  assert_vals ~limit:2 fives [5;5]

let test_fair_disj () =
  let rec fivel x = Trel.(x = int 5 || delay @@ lazy (fivel x)) in
  let rec fiver x = Trel.(delay @@ lazy (fiver x) || x = int 5) in
  assert_vals ~limit:3 fivel [5;5;5];
  assert_vals ~limit:3 fiver [5;5;5];
  ()

let test_unfair_conj () =
  let rec faill x = Trel.(fail && delay @@ lazy (faill x)) in
  assert_find_vals ~limit:3 faill [];
(*  let rec failr x = Trel.(delay @@ lazy (failr x) && fail) in
  assert_find_vals ~limit:3 failr []; *)
  ()

let listo d dl =
  let empty = Trel.(const dl []) in
  let cons x xs =
    Trel.(pure (fun x xs -> x :: xs) |> app d x |> app dl xs |> ret dl)
  in
  let hd l = Trel.(pure List.hd |> app dl l |> ret d) in
  let tl l = Trel.(pure List.tl |> app dl l |> ret dl) in
  let list l = List.fold_right (fun x xs -> cons (Trel.const d x) xs) l empty in
  let conso x xs l = Trel.(cons x xs = l) in
  let heado l x = Trel.(fresh @@ fun xs -> cons x xs = l) in
  let tailo l xs = Trel.(fresh @@ fun x -> cons x xs = l) in
  let rec appendo l0 l1 l =
    (* List.append [] l = l ||
       List.append (x :: xs) l = x :: (List.append xs l) *)
    let open Trel in
    (l0 = empty && l1 = l) ||
    (Fresh.v3 @@ fun x xs tl ->
     (cons x xs) = l0 &&
     (cons x tl) = l  &&
     delay @@ lazy (appendo xs l1 tl))
  in
  let rec revo l r =
    let open Trel in
    (l = empty && r = empty) ||
    (Fresh.v3 @@ fun x xs rt ->
     (cons x xs) = l &&
     delay @@ lazy (revo xs rt) &&
     appendo rt (cons x empty) r)
  in
  empty, cons, hd, tl, list, conso, heado, tailo, appendo, revo

let iempty, icons, ihd, itl, ilist, iconso, iheado, tailo, iappendo, irevo =
  listo Trel.Dom.int Trel.Dom.(list int)

let test_ilist () =
  let il = ilist [1;2;3] in
  let ilh = ihd il in
  let ilt = itl il in
  assert_vals Trel.(fun l -> il = l) [[1;2;3]];
  assert_vals Trel.(fun l -> ilh = l) [1];
  assert_vals Trel.(fun l -> ilt = l) [[2;3]];
  assert_vals (Trel.(fun l -> iconso (Trel.int 1) (ilist [2;3]) l)) [[1;2;3]];
  assert_2vals (fun x xs -> Trel.(iconso x xs (ilist [1;2;3]))) [(1,[2;3])];
  ()

let test_ilist_appendo () =
  assert_vals (fun l -> iappendo (ilist [1;2]) (ilist [3;4]) l) [[1;2;3;4]];
  assert_vals (fun l -> iappendo l (ilist [4]) (ilist [1;2;3;4])) [[1;2;3]];
  assert_vals (fun l -> iappendo (ilist [1;2]) l (ilist [1;2;3;4])) [[3;4]];
  assert (not (Trel.success (iappendo (ilist [1]) (ilist [2]) (ilist [3]))));
  ()

let test_ilist_pre_suf () =
  let pre l pre = Trel.(fresh @@ fun suf -> iappendo pre suf l) in
  let suf l suf = Trel.(fresh @@ fun pre -> iappendo pre suf l) in
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

let test_ilist_revo () =
  assert_vals (fun r -> irevo (ilist [1;2;3]) r) [[3;2;1]];
  ()

let test_pure_unify () =
  let sin x = Trel.(pure sin |> app Dom.float x |> ret Dom.float) in
  let cos x = Trel.(pure cos |> app Dom.float x |> ret Dom.float) in
  assert_vals Trel.(fun x -> cos x = sin x && x = float 0.) [];
  assert_vals Trel.(fun x -> cos x = cos x && x = float 0.) [0.];
  ()

let test () =
  test_simple_unify ();
  test_pair ();
  test_fapp ();
  test_delay ();
  test_fair_disj ();
  test_unfair_conj ();
  test_ilist_appendo ();
  test_ilist_pre_suf ();
  test_ilist_revo ();
  test_pure_unify ();
  print_endline "All tests succeeded!";
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
