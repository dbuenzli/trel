(*---------------------------------------------------------------------------
   Copyright (c) 2017 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let id x = x

let assert_vals ?limit q vals =
  let q = Rel.(var @@ reify q id) in
  assert (Rel.(Seq.to_list ?limit @@ get q) = vals)

module Btree = struct
  type t = Node of int * t * t | Nil
  let rec equal t0 t1 = match t0, t1 with
  | Nil, Nil -> true
  | Node (v0,l0,r0), Node (v1,l1,r1) -> v0 = v1 && equal l0 l1 && equal r0 r1
  | _, _ -> false

  let rec pp ppf = function
  | Nil -> Format.fprintf ppf "Nil"
  | Node (v, l, r) ->
      Format.fprintf ppf "@[Node @[<1>(%d,@ %a,@ %a)@]" v pp l pp r

  let nil = Nil
  let node v l r = Node (v, l, r)

  let rec fold t f z =
    let rec loop acc = function
    | Nil -> z
    | Node (v, l, r) -> f v
    in
    loop z t
end

let dom_btree = Rel.Dom.of_type (module Btree)
let lnil = Rel.const dom_btree Btree.nil
let lnode v l r =
  let open Rel in
  pure Btree.node |> app Dom.int v |> app dom_btree l |> app dom_btree r |>
  ret dom_btree

let rec linj = function
| Btree.Nil -> lnil
| Btree.Node (v, l, r) -> lnode (Rel.int v) (linj l) (linj r)

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
