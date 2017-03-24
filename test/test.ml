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

let p2 = Rel.pair ()
let p2 p =
  Rel.(fresh @@ fun x ->
       fresh @@ fun y ->
       p = (p2 x y) && x = int 5 && y = bool true)
let p2 = Rel.(var @@ reify p2 id)
let () = assert (Rel.(all @@ get p2) = [5, true])

let p3 = Rel.(func @@ arg @@ arg @@ arg @@ constf (fun x y z -> x, y, z))
let p3 p =
  Rel.(fresh @@ fun x ->
       fresh @@ fun y ->
       fresh @@ fun z ->
       p = p3 x y z && x = int 5 && y = bool true && z = string "bla")
let p3 = Rel.(var @@ reify p3 id)
let () = assert (Rel.(all @@ get p3) = [ "bla", true, 5 ])

(* Infinite loop, in fact stack overflow
let rec fives x = Rel.(x = int 5 || fives x)
let fives = Rel.(var @@ reify fives id)
let () = assert (Rel.(all @@ get fives) = [5])
*)

(* These examples are from sokuza-karen
   http://okmij.org/ftp/Scheme/sokuza-kanren.scm *)

let lempty = Rel.const []
let lcons = Rel.(func @@ arg @@ arg @@ constf (fun xs x -> x :: xs))
let lhd = Rel.(func @@ arg @@ constf List.hd)
let ltl = Rel.(func @@ arg @@ constf List.tl)

let intl l = List.fold_right (fun x acc -> lcons (Rel.int x) acc) l lempty

(* N.B. we loose polymorphism *)

let il = intl [1;2;3]
let ilh = lhd il
let ilt = ltl il

(* FIXME we need delay for this

(* Find an element equal to [x] in [l] *)
let rec mem x l =
  Rel.((l = lempty && fail) || (lhd l = x || mem x (ltl l)))

let () = assert (Rel.success @@ mem (Rel.const 2) (intl [1;2;3]))
let () = assert (not (Rel.success @@ mem (Rel.const 10) (intl [1;2;3])))

let els x = mem x (intl [1;2;3])
let els = Rel.(var @@ reify els id)
let () = assert (Rel.(all @@ get els) = [1;2;3])

(* Find an element equal to [x] two lists *)
let common l1 l2 x = Rel.(mem x l1 && mem x l2)

let c0 = Rel.(var @@ reify (common (intl [1;2;3]) (intl [3;4;5])) id)
let () = assert (Rel.(all @@ get c0) = [3])

let c1 = Rel.(var @@ reify (common (intl [1;2;3]) (intl [3;4;1;7])) id)
let () = assert (Rel.(all @@ get c1) = [1;3])

let c2 = Rel.(var @@ reify (common (intl [11;2;3]) (intl [13;4;1;7])) id)
let () = assert (Rel.(all @@ get c2) = [])
*)

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
