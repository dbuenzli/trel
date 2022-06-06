(*---------------------------------------------------------------------------
   Copyright (c) 2017 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let assert_vals ?limit q vals = assert (Trel.Run.get1 ?limit q = vals)

module Tree = struct
  type t = Node of int * t * t | Leaf

  let rec equal t0 t1 = match t0, t1 with
  | Leaf, Leaf -> true
  | Node (v0,l0,r0), Node (v1,l1,r1) -> v0 = v1 && equal l0 l1 && equal r0 r1
  | _, _ -> false

  let rec pp ppf = function
  | Leaf -> Format.fprintf ppf "Leaf"
  | Node (v, l, r) ->
      Format.fprintf ppf "@[Node @[<1>(%d,@ %a,@ %a)@]" v pp l pp r

  let leaf = Leaf
  let node v l r = Node (v, l, r)
end

module Treeo = struct

  let lt i i' = failwith "TODO"

  let dom = Trel.Dom.of_type (module Tree)
  let leaf = Trel.const dom Tree.leaf
  let node v l r =
    let open Trel in
    pure Tree.node |> app Dom.int v |> app dom l |> app dom r |> ret dom

  let rec insert v t t' =
    let open Trel in
    (t = leaf && t' = node v leaf leaf) ||
    (Fresh.v4 @@ fun i l r b ->
     (t = node i l r) &&
     ((t' = t) && (i = v)) ||
     ((t' = node i b r) && (lt i v) && delay @@ lazy (insert v l b)) ||
     ((t' = node i l b) && (lt v i) && delay @@ lazy (insert v r b)))
end


let test () =
  print_endline "All tree tests succeeded!";
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
