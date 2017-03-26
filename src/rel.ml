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

(* Domains

   FIXME try to be more clever about domains for structural types,
   we are generative which is annoying. Give structure to
   tid + hash-consing ? *)

module D = struct

  type 'a t =
    { tid : 'a tid;
      equal : 'a -> 'a -> bool;
      pp : Format.formatter -> 'a -> unit }

  let pp_abstr ppf _ = Format.fprintf ppf "<abstr>"

  let v ?(pp = pp_abstr) ?(equal = ( = )) () =
    let tid = tid () in
    { tid; equal; pp }

  module type V = sig
    type t
    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
  end

  let of_type : type a. (module V with type t = a) -> a t =
  fun (module V) -> v ~pp:V.pp ~equal:V.equal ()

  let with_pp pp d = { d with pp }

  let equal : type a b. a t -> b t -> bool =
  fun d0 d1 -> match teq d0.tid d1.tid with
  | None -> false
  | Some Teq -> true

  let unit =
    let equal = (( = ) : unit -> unit -> bool) in
    let pp ppf v = Format.fprintf ppf "()" in
    v ~pp ~equal ()

  let bool =
    let equal = (( = ) : bool -> bool -> bool) in
    let pp = Format.pp_print_bool in
    v ~pp ~equal ()

  let int =
    let equal = (( = ) : int -> int -> bool) in
    let pp = Format.pp_print_int in
    v ~pp ~equal ()

  let float =
    let equal = (( = ) : float -> float -> bool) in
    let pp = Format.pp_print_float in
    v ~pp ~equal ()

  let string =
    let equal = (( = ) : string -> string -> bool) in
    let pp = Format.pp_print_string in
    v ~pp ~equal ()

  let pair f s =
    let equal (f0, s0) (f1, s1) = f.equal f0 f1 && s.equal s0 s1 in
    let pp ppf (fv, sv) =
      Format.fprintf ppf "@[<1>(%a,@, %a)@]" f.pp fv s.pp sv
    in
    v ~pp ~equal ()

  let list e =
    let equal l0 l1 = List.for_all2 e.equal l0 l1 in
    let pp ppf l =
      let pp_sep ppf () = Format.fprintf ppf ";@ " in
      Format.fprintf ppf "@[<1>[%a]@]" (Format.pp_print_list ~pp_sep e.pp) l
    in
    v ~pp ~equal ()
end

type 'a dom = 'a D.t
let dom = D.v

(* Terms

   N.B. We type function applications rather than pure values and
   variables. This allows to keep variables untyped which seems to
   lead to a more lightweight edsl. *)

type 'a var = 'a term Hmap.key
and 'a tapp = 'a
and 'a term =
| Var : 'a var -> 'a term
| Pure : 'a -> 'a tapp term
| App : ('a -> 'b) tapp term * 'a dom * 'a term -> 'b tapp term
| Ret : 'a dom * 'a tapp term -> 'a term

(* Variables *)

let new_var : unit -> 'a var = fun () -> Hmap.Key.create ()
let eq_var : 'a 'b. 'a term -> 'b term -> bool =
fun t0 t1 -> match t0, t1 with
| Var v0, Var v1 -> Hmap.Key.(equal (hide_type v0) (hide_type v1))
| _ -> false

(* Constants *)

let const dom v = Ret (dom, Pure v)
let unit = const D.unit ()
let bool = const D.bool
let int = const D.int
let float = const D.float
let string = const D.string

(* Functions *)

type 'a app = 'a tapp term

let pure f = Pure f
let app dom v app = App (app, dom, v)
let ret dom app = Ret (dom, app)

let pair fst snd = (fst, snd)
let pair fdom sdom tdom =
  fun fst snd -> pure pair |> app fdom fst |> app sdom snd |> ret tdom

(* Unification *)

type subst = Hmap.t

let rec walk : 'a term -> subst -> 'a term =
fun t s -> match t with
| Var v -> (match Hmap.find v s with None -> t | Some v -> walk v s)
| t -> t

let rec unify : type a. a term -> a term -> subst -> subst option =
fun t0 t1 s -> match walk t0 s, walk t1 s with
| (Var _ as v0), (Var _ as v1) when eq_var v0 v1 -> Some s
| Var v, t | t, Var v -> Some (Hmap.add v t s)
| Pure _, Pure _ -> Some s (* must unify by semantics *)
| Ret (d0, app0), Ret (d1, app1) ->
    if not (d0.D.tid == d1.D.tid) then None else
    begin match app0, app1 with
    | Pure v0, Pure v1 -> if d0.D.equal v0 v1 then Some s else None
    | App _, App _ -> unify app0 app1 s
    | _, _ -> None
    end
| App (f0, d0, v0), App (f1, d1, v1) ->
    begin match teq d0.D.tid d1.D.tid with
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

type 'a stream =
| Empty
| Mature of 'a * 'a stream
| Immature of 'a stream Lazy.t

let rec is_empty s = match s with
| Empty -> true
| Mature _ -> false
| Immature s -> is_empty (Lazy.force s)

let rec head = function
| Empty -> invalid_arg "Empty stream"
| Mature (v, s) -> v
| Immature s -> head (Lazy.force s)

let rec next = function
| Empty -> None
| Mature (v, s) -> Some (v, s)
| Immature s -> next (Lazy.force s)

let all s =
  let rec loop acc = function
  | Empty -> List.rev acc
  | Mature (v, s) -> loop (v :: acc) s
  | Immature s -> loop acc (Lazy.force s)
  in
  loop [] s

let rec smplus s0 s1 = match s0 with
| Empty -> s1
| Mature (v, s0) -> Mature (v, Immature (lazy (smplus s1 s0)))
| Immature s0 -> Immature (lazy (smplus s1 (Lazy.force s0)))

let rec sbind s f = match s with
| Empty -> Empty
| Mature (v, s) -> smplus (f v) (sbind s f)
| Immature s -> Immature (lazy (sbind (Lazy.force s) f))

let rec smap f s = match s with
| Empty -> Empty
| Mature (v, s) -> Mature (f v, smap f s)
| Immature s -> Immature (lazy (smap f (Lazy.force s)))

(* Goals *)

type goal = state -> state stream
let fail _ = Empty
let succeed s = Mature (s, Empty)

let ( = ) : 'a term -> 'a term -> goal =
fun t0 t1 st -> match unify t0 t1 st with
| None -> Empty
| Some st -> succeed st

let fresh : ('a term -> goal) -> goal =
fun f st -> f (Var (new_var ())) st

let ( || ) : goal -> goal -> goal = fun g0 g1 st -> smplus (g0 st) (g1 st)
let ( && ) : goal -> goal -> goal = fun g0 g1 st -> sbind (g0 st) g1

let rec success g = not (is_empty (g empty))

let delay gazy st = Immature (lazy ((Lazy.force gazy) st))

(* Reifying goals *)

let rec term_value : type a. a term -> subst -> a =
fun t s -> match t with
| Var v ->
    begin match Hmap.find v s with
    | Some t -> term_value t s
    | _ -> raise Exit
    end
| Pure v -> v
| Ret (_, t) -> term_value t s
| App (f, _, v) ->
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

let _find (goal, proj) = smap proj (goal empty)

let find reify = try Ok (_find reify) with Exit -> Error `Undefined
let get reify = try _find reify with
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
