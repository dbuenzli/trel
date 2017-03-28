(* These examples are in the public domain *)


let q x = Rel.(x = int 5)
let xs = Rel.(Seq.to_list @@ run @@ Query.v1 @@ reifier q Value.get1)
let () = assert (xs = [5])

let q x y = Rel.(y = int 6 && (x = y || x = int 5))
let xys = Rel.(Seq.to_list @@ run @@ Query.v2 @@ reifier q Value.get2)
let () = assert (xys = [(6, 6); (5, 6)])

(* int lists *)

let intl = Rel.Dom.(list int)

let empty = Rel.const intl []
let cons x xs = Rel.(pure List.cons |> app Dom.int x |> app intl xs |> ret intl)

let rec ilist = function [] -> empty | i :: is -> cons (Rel.int i) (ilist is)

let l x xs = Rel.(cons x xs = ilist [1;2;3])
let ls = Rel.(Seq.to_list @@ run @@ Query.v2 @@ reifier l Value.get2)
let () = assert (ls = [(1, [2;3])])

(* Relational append *)

let rec appendo l0 l1 l =
  let open Rel in
  (l0 = empty && l1 = l) ||
  (Fresh.v3 @@ fun x xs tl ->
   cons x xs = l0 &&
   cons x tl = l  &&
   delay @@ lazy (appendo xs l1 tl))
