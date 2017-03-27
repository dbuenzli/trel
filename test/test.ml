(*---------------------------------------------------------------------------
   Copyright (c) 2017 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let assert_vals ?limit q vals =
  let q = Rel.(var @@ reifier q Value.get) in
  assert (Rel.(Seq.to_list ?limit @@ run q) = vals)

let assert_2vals ?limit q vals =
  let reify x y = Rel.Value.(get x, get y) in
  let q = Rel.(var @@ var @@ reifier q reify) in
  assert (Rel.(Seq.to_list ?limit @@ run q) = vals)

let test_simple_unify () =
  assert_vals (fun q -> Rel.(q = int 5)) [5];
  assert_vals (fun q -> Rel.(q = int 5 || q = int 6)) [5;6];
  assert_2vals
    (fun a b -> Rel.(a = int 7 && (b = int 5 || b = int 6))) [(7, 5); (7, 6)];
  assert_vals
    (fun x -> Rel.(Fresh.v2 @@ fun y z -> x = y && y = z && z = int 3)) [3];
  ()

let test_pair () =
  let p2 = Rel.(pair Dom.int Dom.bool Dom.(pair int bool)) in
  let p p =
    let open Rel in
    Fresh.v2 @@ fun x y ->
    p = (p2 x y) && x = int 5 && y = bool true
  in
  assert_vals p [5, true];
  ()

let test_fapp () =
  let triple x y z = (x, y, z) in
  let t3 x y z =
    let open Rel in
    pure triple |> app Dom.int x |> app Dom.bool y |> app Dom.string z |>
    ret (Dom.v ())
  in
  let t t =
    let open Rel in
    Fresh.v3 @@ fun x y z ->
    t = t3 x y z && x = int 5 && y = bool true && z = string "bla"
  in
  assert_vals t [(5, true, "bla")];
  ()

let test_delay () =
  let rec fives x = Rel.(x = int 5 || delay (lazy (fives x))) in
  assert_vals ~limit:2 fives [5;5]

let listo d dl =
  let empty = Rel.(const dl []) in
  let cons x xs =
    Rel.(pure (fun x xs -> x :: xs) |> app d x |> app dl xs |> ret dl)
  in
  let hd l = Rel.(pure List.hd |> app dl l |> ret d) in
  let tl l = Rel.(pure List.tl |> app dl l |> ret dl) in
  let list l = List.fold_right (fun x xs -> cons (Rel.const d x) xs) l empty in
  let conso x xs l = Rel.(cons x xs = l) in
  let heado l x = Rel.(fresh @@ fun xs -> cons x xs = l) in
  let tailo l xs = Rel.(fresh @@ fun x -> cons x xs = l) in
  let rec appendo l0 l1 l =
    (* List.append [] l = l ||
       List.append (x :: xs) l = x :: (List.append xs l) *)
    let open Rel in
    (l0 = empty && l1 = l) ||
    (Fresh.v3 @@ fun x xs tl ->
     (cons x xs) = l0 &&
     (cons x tl) = l  &&
     delay @@ lazy (appendo xs l1 tl))
  in
  let rec revo l r =
    let open Rel in
    (l = empty && r = empty) ||
    (Fresh.v3 @@ fun x xs rt ->
     (cons x xs) = l &&
     delay @@ lazy (revo xs rt) &&
     appendo rt (cons x empty) r)
  in
  empty, cons, hd, tl, list, conso, heado, tailo, appendo, revo


let iempty, icons, ihd, itl, ilist, iconso, iheado, tailo, iappendo, irevo =
  listo Rel.Dom.int Rel.Dom.(list int)

let test_ilist () =
  let il = ilist [1;2;3] in
  let ilh = ihd il in
  let ilt = itl il in
  assert_vals Rel.(fun l -> il = l) [[1;2;3]];
  assert_vals Rel.(fun l -> ilh = l) [1];
  assert_vals Rel.(fun l -> ilt = l) [[2;3]];
  assert_vals (Rel.(fun l -> iconso (Rel.int 1) (ilist [2;3]) l)) [[1;2;3]];
  assert_2vals (fun x xs -> Rel.(iconso x xs (ilist [1;2;3]))) [(1,[2;3])];
  ()

let test_ilist_appendo () =
  assert_vals (fun l -> iappendo (ilist [1;2]) (ilist [3;4]) l) [[1;2;3;4]];
  assert_vals (fun l -> iappendo l (ilist [4]) (ilist [1;2;3;4])) [[1;2;3]];
  assert_vals (fun l -> iappendo (ilist [1;2]) l (ilist [1;2;3;4])) [[3;4]];
  assert (not (Rel.success (iappendo (ilist [1]) (ilist [2]) (ilist [3]))));
  ()

let test_ilist_pre_suf () =
  let pre l pre = Rel.(fresh @@ fun suf -> iappendo pre suf l) in
  let suf l suf = Rel.(fresh @@ fun pre -> iappendo pre suf l) in
  let pre_suf l pre suf = iappendo pre suf l in
  let l = ilist [1;2;3;4] in
  assert_vals (pre l) [[]; [1]; [1;2]; [1;2;3]; [1;2;3;4]];
  assert_vals (suf l) [[1;2;3;4]; [2;3;4]; [3;4]; [4]; []];
  let pre_suf = pre_suf l in
  let pre_suf = Rel.(var @@ var @@
                     reifier pre_suf (fun p s -> Rel.Value.(get p, get s)))
  in
  assert (Rel.(Seq.to_list @@ run pre_suf) =
          [([], [1;2;3;4]);
           ([1], [2;3;4]);
           ([1;2], [3;4]);
           ([1;2;3], [4]);
           ([1;2;3;4], [])]);
  ()

let test_ilist_revo () =
  assert_vals (fun r -> irevo (ilist [1;2;3]) r) [[3;2;1]];
  ()

let test () =
  test_simple_unify ();
  test_pair ();
  test_fapp ();
  test_delay ();
  test_ilist_appendo ();
  test_ilist_pre_suf ();
  test_ilist_revo ();
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
