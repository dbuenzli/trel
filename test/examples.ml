(* These examples are in the public domain *)


let q x = Trel.(x = int 5)
let xs = Trel.(Seq.to_list @@ run @@ Query.v1 @@ reifier q Value.get1)
let () = assert (xs = [5])

let q x y = Trel.(y = int 6 && (x = y || x = int 5))
let xys = Trel.(Seq.to_list @@ run @@ Query.v2 @@ reifier q Value.get2)
let () = assert (xys = [(6, 6); (5, 6)])

(* int lists *)

let intl = Trel.Dom.(list int)

let empty = Trel.const intl []
let cons x xs = Trel.(pure List.cons |> app Dom.int x |> app intl xs |> ret intl)

let rec ilist = function [] -> empty | i :: is -> cons (Trel.int i) (ilist is)

let l x xs = Trel.(cons x xs = ilist [1;2;3])
let ls = Trel.(Seq.to_list @@ run @@ Query.v2 @@ reifier l Value.get2)
let () = assert (ls = [(1, [2;3])])

(* Trelational append *)

let rec appendo l0 l1 l =
  let open Trel in
  (l0 = empty && l1 = l) ||
  (Fresh.v3 @@ fun x xs tl ->
   cons x xs = l0 &&
   cons x tl = l  &&
   delay @@ lazy (appendo xs l1 tl))
