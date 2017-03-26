(*---------------------------------------------------------------------------
   Copyright (c) 2017 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let id x = x

let q q = Rel.(q = int 5)
let q = Rel.(var @@ reify q id)
let () = assert (Rel.(all @@ get q) = [5])

let x x =
  Rel.(fresh @@ fun y ->
       fresh @@ fun z ->
       x = y && y = z && z = int 3)
let x = Rel.(var @@ reify x id)
let () = assert (Rel.(all @@ get x) = [3])

let ab_inj a b = Rel.(a = int 7 && (b = int 5 || b = int 6))
let ab_prj a b = (a, b)
let ab = Rel.(var @@ var @@ reify ab_inj ab_prj)
let () = assert (Rel.(all @@ get ab) = [(7, 5); (7, 6)])

let p2 = Rel.(pair Dom.int Dom.bool Dom.(pair int bool))
let p2 p =
  Rel.(fresh @@ fun x ->
       fresh @@ fun y ->
       p = (p2 x y) && x = int 5 && y = bool true)
let p2 = Rel.(var @@ reify p2 id)
let () = assert (Rel.(all @@ get p2) = [5, true])

let triple x y z = (x, y, z)
let p3 x y z = Rel.(pure triple |>
                    app Dom.int x |> app Dom.bool y |> app Dom.string z |>
                    ret (Dom.v ()))
let p3 p =
  Rel.(fresh @@ fun x ->
       fresh @@ fun y ->
       fresh @@ fun z ->
       p = p3 x y z && x = int 5 && y = bool true && z = string "bla")
let p3 = Rel.(var @@ reify p3 id)
let () = assert (Rel.(all @@ get p3) = [ 5, true, "bla" ])

let rec fives x = Rel.(x = int 5 || delay (lazy (fives x)))
let fives = Rel.(var @@ reify fives id)
let () = assert (Rel.(head @@ get fives) = 5)

(* These examples are from sokuza-karen
   http://okmij.org/ftp/Scheme/sokuza-kanren.scm *)

let listo d dl =
  let empty = Rel.(const dl []) in
  let cons x xs =
    Rel.(pure (fun x xs -> x :: xs) |> app d x |> app dl xs |> ret dl)
  in
  let hd l = Rel.(pure List.hd |> app dl l |> ret d) in
  let tl l = Rel.(pure List.tl |> app dl l |> ret dl) in
  let list l = List.fold_right (fun x xs -> cons (Rel.const d x) xs) l empty in
  let conso x xs l = Rel.(cons x xs = l) in
  empty, cons, hd, tl, list, conso


let iempty, icons, ihd, itl, ilist, iconso =
  listo Rel.Dom.int Rel.Dom.(list int)

let il = ilist [1;2;3]
let ilh = ihd il
let ilt = itl il

let () =
  assert (Rel.(all @@ get (Rel.var @@ reify (fun l -> il = l) id)) = [[1;2;3]]);
  assert (Rel.(all @@ get (Rel.var @@ reify (fun l -> ilh = l) id)) = [1]);
  assert (Rel.(all @@ get (Rel.var @@ reify (fun l -> ilt = l) id)) = [[2;3]]);
  ()

let l l = Rel.(iconso (Rel.int 1) (ilist [2;3]) l)
let l = Rel.(var @@ reify l id)
let () = assert (Rel.(all @@ get l) = [[1;2;3]])

let m4tch x xs = Rel.(iconso x xs (ilist [1;2;3]))
let m4tch = Rel.(var @@ var @@ reify m4tch (fun x xs -> x, xs))
let () = assert (Rel.(all @@ get m4tch) = [(1, [2;3])])

let rec appendo l0 l1 l =
  (* List.append [] l = l ||
     List.append (x :: xs) l = x :: (List.append xs l) *)
  Rel.((l0 = iempty && l1 = l) ||
       (fresh @@ fun x ->
        fresh @@ fun xs ->
        fresh @@ fun l' ->
        iconso x xs l0 &&
        iconso x l' l &&
        delay @@ lazy (appendo xs l1 l')))

let l l = appendo (ilist [1;2;3]) (ilist [4;5;6]) l
let l = Rel.(var @@ reify l id)
let () = assert (Rel.(all @@ get l) = [[1;2;3;4;5;6]])

let l l = appendo l (ilist [4;5;6]) (ilist [1;2;3;4;5;6])
let l = Rel.(var @@ reify l id)
let () = assert (Rel.(all @@ get l) = [[1;2;3]])

let l l = appendo (ilist [1;2]) l (ilist [1;2;3;4;5;6])
let l = Rel.(var @@ reify l id)
let () = assert (Rel.(all @@ get l) = [[3;4;5;6]])

let l = appendo (ilist [1]) (ilist [2]) (ilist [3])
let () = assert (not (Rel.success l))

let l pre =
  Rel.(fresh @@ fun suf ->
       appendo pre suf (ilist [1;2;3;4]))

let l = Rel.(var @@ reify l id)
let () = assert (Rel.(all @@ get l) = [[]; [1]; [1;2]; [1;2;3]; [1;2;3;4]])

let l suf =
  Rel.(fresh @@ fun pre ->
       appendo pre suf (ilist [1;2;3;4]))

let l = Rel.(var @@ reify l id)
let () = assert (Rel.(all @@ get l) = [[1;2;3;4]; [2;3;4]; [3;4]; [4]; []])

let l pre suf = appendo pre suf (ilist [1;2;3;4])
let l = Rel.(var @@ var @@ reify l (fun p s -> (p, s)))
let () = assert (Rel.(all @@ get l) =
                 [([], [1;2;3;4]);
                  ([1], [2;3;4]);
                  ([1;2], [3;4]);
                  ([1;2;3], [4]);
                  ([1;2;3;4], [])])

let () = print_endline "All tests succeeded!"

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
