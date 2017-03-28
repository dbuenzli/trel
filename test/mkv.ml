(*---------------------------------------------------------------------------
   Copyright (c) 2017 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Straightforward minimal typed implementation of μKanren. Without
   bells and whistles, based on the untyped version described in:

   Jason Hemann and Daniel P. Friedman.
   microKanren: A Minimal Functional Core for Relational Programming.
   In Proceedings of the 2013 Workshop on Scheme and Functional Programming
   (Scheme '13), Alexandria, VA, 2013. *)

(* Lazy sequences of values *)

type 'a seq = Empty | Cons of 'a * 'a seq | Delay of 'a seq Lazy.t

let seq_to_list ?limit s =
  let limit = match limit with
  | Some l when l < 0 -> invalid_arg (Printf.sprintf "negative limit (%d)" l)
  | Some l -> l
  | None -> -1
  in
  let rec loop limit acc s = match limit = 0 with
  | true -> List.rev acc
  | false ->
      match s with
      | Empty -> List.rev acc
      | Delay s -> loop limit acc (Lazy.force s)
      | Cons (v, s) ->
          let limit = if limit = -1 then limit else limit - 1 in
          loop limit (v :: acc) s
  in
  loop limit [] s

let rec seq_mplus s0 s1 = match s0 with
| Empty -> s1
| Cons (v, s0) -> Cons (v, Delay (lazy (seq_mplus s1 s0)))
| Delay s0 -> Delay (lazy (seq_mplus s1 (Lazy.force s0)))

let rec seq_bind s f = match s with
| Empty -> Empty
| Cons (v, s) -> seq_mplus (f v) (seq_bind s f)
| Delay s -> Delay (lazy (seq_bind (Lazy.force s) f))

let rec seq_map f s = match s with
| Empty -> Empty
| Cons (v, s) -> Cons (f v, seq_map f s)
| Delay s -> Delay (lazy (seq_map f (Lazy.force s)))

(* Type identifiers *)

module Tid = struct type _ t = .. end
module type Tid = sig
  type t
  type _ Tid.t += Tid : t Tid.t
end
type 'a tid = (module Tid with type t = 'a)

let tid () (type s) =
  let module M = struct
    type t = s
    type _ Tid.t += Tid : t Tid.t
  end
  in
  (module M : Tid with type t = s)

type ('a, 'b) teq = Teq : ('a, 'a) teq

let teq : type r s. r tid -> s tid -> (r, s) teq option =
  fun r s ->
    let module R = (val r : Tid with type t = r) in
    let module S = (val s : Tid with type t = s) in
    match R.Tid with
    | S.Tid -> Some Teq
    | _ -> None

(* Domains *)

type 'a dom = { tid : 'a tid; equal : 'a -> 'a -> bool; }
let dom ~equal = { tid = tid (); equal }

(* Variables *)

type 'a var = { id : int; dom : 'a dom; }

module Var = struct
  type t = V : 'a var -> t
  let compare (V v0) (V v1) = (compare : int -> int -> int) v0.id v1.id
end

module Vmap = Map.Make (Var)

(* Terms *)

type 'a term =
| Var of 'a var
| Ret of 'a dom * 'a ret

and 'a ret =
| App : ('a -> 'b) ret * 'a term -> 'b ret
| Pure : 'a -> 'a ret

let var dom id = Var { id; dom }
let const dom v = Ret (dom, Pure v)
let pure f = Pure f
let app v ret = App (ret, v)
let ret dom ret = Ret (dom, ret)

(* Substitutions *)

type binding = B : 'a var * 'a term -> binding
type subst = binding Vmap.t

let subst_empty = Vmap.empty
let subst_add var t s = Vmap.add (Var.V var) (B (var, t)) s
let subst_find : type a. a var -> subst -> a term option = fun v s ->
  try
    let B (v', t) = Vmap.find (Var.V v) s in
    match teq v.dom.tid v'.dom.tid with None -> None | Some Teq -> Some t
  with Not_found -> None

(* Unification *)

let rec walk t s = match t with
| Var v -> (match subst_find v s with None -> t | Some v -> walk v s)
| t -> t

let term_dom = function Var v -> v.dom | Ret (d, _) -> d

let rec unify : type a. a term -> a term -> subst -> subst option =
fun t0 t1 s -> match walk t0 s, walk t1 s with
| Var v0, Var v1 when v0.id = v1.id -> Some s
| Var v, t | t, Var v -> Some (subst_add v t s)
| Ret (d0, r0), Ret (d1, r1) ->
    if not (d0.tid == d1.tid) then None else
    match r0, r1 with
    | Pure v0, Pure v1 -> if d0.equal v0 v1 then Some s else None
    | App _, App _ -> unify_ret r0 r1 s
    | _, _ -> None

and unify_ret : type a. a ret -> a ret -> subst -> subst option =
fun r0 r1 s -> match r0, r1 with
| App (f0, v0), App (f1, v1) ->
    begin match teq (term_dom v0).tid (term_dom v1).tid with
    | None -> None
    | Some Teq ->
        match unify v0 v1 s with
        | None -> None
        | Some s -> unify_ret f0 f1 s
    end
| Pure f0, Pure f1 when f0 == f1 -> Some s
| _, _ -> None

(* State *)

type state = { next_vid : int; subst : subst }
let state_empty = { next_vid = 0; subst = subst_empty }

(* Goals *)

type goal = state -> state seq

let fail _ = Empty
let succeed st = Cons (st, Empty)

let ( = ) t0 t1 st = match unify t0 t1 st.subst with
| None -> Empty
| Some subst -> succeed { st with subst }

let fresh dom lambda st =
  let var = var dom st.next_vid in
  lambda var { st with next_vid = st.next_vid + 1 }

let ( || ) g0 g1 st = seq_mplus (g0 st) (g1 st)
let ( && ) g0 g1 st = seq_bind (g0 st) g1
let delay gazy st = Delay (lazy ((Lazy.force gazy) st))

(* Reification *)

let term_value t s =
  let rec term_value : type a. a term -> subst -> a =
  fun t s -> match t with
  | Ret (d, t) -> ret_value t s
  | Var v ->
      match subst_find v s with
      | Some t -> term_value t s
      | None -> raise Exit
  and ret_value : type a. a ret -> subst -> a =
  fun r s -> match r with
  | Pure v -> v
  | App (f, v) -> (ret_value f s) (term_value v s)
  in
  try Some (term_value t s) with Exit -> None

type 'a value = 'a term * subst

let value_find (var, subst) = term_value var subst
let value_get (var, subst) = match term_value var subst with
| None -> invalid_arg "undefined value"
| Some v -> v

type ('q, 'r) reifier = { next_vid : int; query : 'q; reify : state -> 'r }

let reifier query reify = { next_vid = 0; query; reify = (fun _ -> reify) }
let query dom r =
  let var = var dom r.next_vid in
  let query = r.query var in
  let reify st = r.reify st (var, st.subst) in
  let next_vid = r.next_vid + 1 in
  { next_vid; query; reify }

let run r = seq_map r.reify (r.query { state_empty with next_vid = r.next_vid })

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
