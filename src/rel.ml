(*---------------------------------------------------------------------------
   Copyright (c) 2017 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Straightforward typed implementation of μKanren. See:

   Jason Hemann and Daniel P. Friedman.
   microKanren: A Minimal Functional Core for Relational Programming.
   In Proceedings of the 2013 Workshop on Scheme and Functional Programming
   (Scheme '13), Alexandria, VA, 2013. *)

(* Type identifiers
   See http://alan.petitepomme.net/cwn/2015.03.24.html#1 *)

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

(* Terms *)

type 'a var = 'a term Hmap.key
and 'a term =
| Var : 'a var -> 'a term
| Const : ('a -> 'a -> bool) * 'a -> 'a term
| App : 'a tid * ('a -> 'b) term * 'a term -> 'b term

let new_var : unit -> 'a var = fun () -> Hmap.Key.create ()
let eq_var : 'a 'b. 'a term -> 'b term -> bool =
fun t0 t1 -> match t0, t1 with
| Var v0, Var v1 -> Hmap.Key.(equal (hide_type v0) (hide_type v1))
| _ -> false

(* Constants *)

let const ?(eq = ( = )) v = Const (eq, v)
let constp v = const ~eq:(( == )) v
let unit = const ~eq:(( = ) : unit -> unit -> bool) ()
let bool = const ~eq:(( = ) : bool -> bool -> bool)
let int = const ~eq:(( = ) : int -> int -> bool)
let string = const ~eq:(( = ) : string -> string -> bool)

let pair () x y =
  let pair x y = (x, y) in
  let tid0 = tid () in
  let tid1 = tid () in
  App (tid1, App (tid0, (constp pair), x), y)

type ('a, 'b) func = 'a -> 'b

let constf f k = k (constp f)
let arg step =
  let tid = tid () in
  (fun k x -> step (fun f -> k (App (tid, f, x))))

let func step = step (fun x -> x)

(* Unification *)

type subst = Hmap.t

let rec walk : 'a term -> subst -> 'a term =
fun t s -> match t with
| Var v -> (match Hmap.find v s with None -> t | Some v -> walk v s)
| t -> t

let rec unify : type a. a term -> a term -> subst -> subst option =
fun t0 t1 s -> match walk t0 s, walk t1 s with
| (Var _ as v0), (Var _ as v1) when eq_var v0 v1 -> Some s
| (Var v), t | t, (Var v) -> Some (Hmap.add v t s)
| Const (eq0, v0), Const (eq1, v1) ->
    if not (eq0 == eq1) then assert false else
    if eq0 v0 v1 then Some s else None
| App (tid0, f0, v0), App (tid1, f1, v1) ->
    begin match teq tid0 tid1 with
    | None -> None
    | Some Teq ->
        begin match unify v0 v1 s with
        | None -> None
        | Some s -> unify f0 f1 s
        end
    end
| _, _ -> None

(* State *)

type state = subst
let empty = Hmap.empty

(* Streams *)

type stream = state list
let mplus : stream -> stream -> stream = List.append
let rec bind : stream -> (state -> stream) -> stream =
fun sts g -> match sts with
| [] -> []
| st :: sts -> mplus (g st) (bind sts g)

(* Goals *)

type goal = state -> stream
let fail s = []
let succeed s = [s]

let ( = ) : 'a term -> 'a term -> goal =
fun t0 t1 st -> match unify t0 t1 st with
| None -> []
| Some st -> succeed st

let fresh : ('a term -> goal) -> goal =
fun f st -> f (Var (new_var ())) st

let ( || ) : goal -> goal -> goal = fun g0 g1 st -> mplus (g0 st) (g1 st)
let ( && ) : goal -> goal -> goal = fun g0 g1 st -> bind (g0 st) g1

let success g = g empty <> []

(* Sequences *)

type 'a seq = 'a list
let next = function
| [] -> None
| a :: ls -> Some (a, ls)

let all r = r

(* Reifying goals *)

let rec term_value : type a. a term -> subst -> a =
fun t s -> match t with
| Const (_, v) -> v
| Var v ->
    begin match Hmap.find v s with
    | Some t -> term_value t s
    | _ -> raise Exit
    end
| App (_, f, v) ->
    let f = term_value f s in
    let v = term_value v s in
    f v

type ('a, 'b) reify = 'a * (state -> 'b)

let reify inj proj = inj, (fun _ -> proj)

let var (inj, proj) =
  let var = Var (new_var ()) in
  let inj = inj var in
  let proj s = proj s (term_value var s) in
  (inj, proj)

let _find (goal, proj) =
  let ss = goal empty in
  let add acc s = proj s :: acc in
  List.rev (List.fold_left add [] ss)

let find fnd = try Ok (_find fnd) with Exit -> Error `Undefined
let get fnd = try _find fnd with
| Exit -> invalid_arg "a result variable is undefined"

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
