(*---------------------------------------------------------------------------
   Copyright (c) 2017 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let strf = Format.asprintf

(* Lazy sequences of values. *)

module Seq = struct
  type 'a t =
  | Empty
  | Cons of 'a * 'a t
  | Delay of 'a t Lazy.t

  let empty = Empty
  let cons v s = Cons (v, s)
  let delay sazy = Delay sazy

  let rec is_empty s = match s with
  | Empty -> true
  | Cons _ -> false
  | Delay s -> is_empty (Lazy.force s)

  let rec head = function
  | Empty -> None
  | Cons (v, s) -> Some v
  | Delay s -> head (Lazy.force s)

  let rec get_head = function
  | Empty -> invalid_arg "Sequence is empty"
  | Cons (v, s) -> v
  | Delay s -> get_head (Lazy.force s)

  let rec tail = function
  | Empty -> invalid_arg "Sequence is empty"
  | Cons (v, s) -> s
  | Delay s -> tail (Lazy.force s)

  let to_list ?limit s =
    let limit = match limit with
    | Some l when l < 0 -> invalid_arg (strf "negative limit (%d)" l)
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

  let rec mplus s0 s1 = match s0 with
  | Empty -> s1
  | Cons (v, s0) -> Cons (v, Delay (lazy (mplus s1 s0)))
  | Delay s0 -> Delay (lazy (mplus s1 (Lazy.force s0)))

  let rec bind s f = match s with
  | Empty -> Empty
  | Cons (v, s) -> mplus (f v) (bind s f)
  | Delay s -> Delay (lazy (bind (Lazy.force s) f))

  let rec map f s = match s with
  | Empty -> Empty
  | Cons (v, s) -> Cons (f v, map f s)
  | Delay s -> Delay (lazy (map f (Lazy.force s)))
end

type 'a seq = 'a Seq.t

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

module Dom = struct

  type 'a t =
    { name : string;
      tid : 'a tid;
      equal : 'a -> 'a -> bool;
      pp : Format.formatter -> 'a -> unit }

  let pp_abstr ppf _ = Format.fprintf ppf "<abstr>"

  let v ?(name = "unknown") ?(pp = pp_abstr) ?(equal = ( = )) () =
    let tid = tid () in
    { name; tid; equal; pp }

  module type V = sig
    type t
    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
  end

  let of_type : type a. (module V with type t = a) -> a t =
  fun (module V) -> v ~pp:V.pp ~equal:V.equal ()

  let with_dom ?name ?pp d =
    let name = match name with None -> d.name | Some n -> n in
    let pp = match pp with None -> d.pp | Some pp -> pp in
    { d with name; pp }

  let name d = d.name
  let pp_value d = d.pp
  let equal_value d = d.equal

  let equal : type a b. a t -> b t -> bool =
  fun d0 d1 -> match teq d0.tid d1.tid with
  | None -> false
  | Some Teq -> true

  let pp ppf d = Format.pp_print_string ppf d.name

  (* Predefined domains *)

  let unit =
    let equal = (( = ) : unit -> unit -> bool) in
    let pp ppf v = Format.fprintf ppf "()" in
    v ~name:"unit" ~pp ~equal ()

  let bool =
    let equal = (( = ) : bool -> bool -> bool) in
    let pp = Format.pp_print_bool in
    v ~name:"bool" ~pp ~equal ()

  let int =
    let equal = (( = ) : int -> int -> bool) in
    let pp = Format.pp_print_int in
    v ~name:"int" ~pp ~equal ()

  let float =
    let equal = (( = ) : float -> float -> bool) in
    let pp = Format.pp_print_float in
    v ~name:"float" ~pp ~equal ()

  let string =
    let equal = (( = ) : string -> string -> bool) in
    let pp = Format.pp_print_string in
    v ~name:"string" ~pp ~equal ()

  let pair f s =
    let equal (f0, s0) (f1, s1) = f.equal f0 f1 && s.equal s0 s1 in
    let pp ppf (fv, sv) =
      Format.fprintf ppf "@[<1>(%a,@, %a)@]" f.pp fv s.pp sv
    in
    let name = strf "%s * %s" f.name s.name in
    v ~name ~pp ~equal ()

  let list e =
    let equal l0 l1 = List.for_all2 e.equal l0 l1 in
    let pp ppf l =
      let pp_sep ppf () = Format.fprintf ppf ";@ " in
      Format.fprintf ppf "@[<1>[%a]@]" (Format.pp_print_list ~pp_sep e.pp) l
    in
    let name = match String.contains e.name ' ' with
    | true -> strf "(%s) list" e.name
    | false -> strf "%s list" e.name
    in
    v ~name ~pp ~equal ()
end

type 'a dom = 'a Dom.t

(* Variables *)

type 'a var = { id : int; name : string option; tid : 'a tid; }

module Var = struct
  type t = V : 'a var -> t
  let compare (V v0) (V v1) = (compare : int -> int -> int) v0.id v1.id
end

(* Terms

   N.B. We type function applications rather than pure values and
   variables. This allows to keep variables untyped which seems to
   lead to a more lightweight edsl. *)

type 'a term =
| Var of 'a var
| Ret of 'a dom * 'a ret

and 'a ret =
| App : ('a -> 'b) ret * 'a dom * 'a term -> 'b ret
| Pure : 'a -> 'a ret

let var ?name id = Var { id; name; tid = tid () }
let const dom v = Ret (dom, Pure v)
let pure f = Pure f
let app dom v ret = App (ret, dom, v)
let ret dom ret = Ret (dom, ret)

let unit = const Dom.unit ()
let bool = const Dom.bool
let int = const Dom.int
let float = const Dom.float
let string = const Dom.string

let pair fst snd = (fst, snd)
let pair fdom sdom tdom =
  fun fst snd -> pure pair |> app fdom fst |> app sdom snd |> ret tdom

let pp_var ppf v = match v.name with
| Some n -> Format.fprintf ppf "%s" n
| None -> Format.fprintf ppf "_%d" v.id

let rec pp_term : type a. Format.formatter -> a term -> unit =
(* FIXME not T.R. *)
fun ppf -> function
| Var v -> pp_var ppf v
| Ret (d, ret) ->
    match ret with
    | Pure v -> d.Dom.pp ppf v
    | App _ as ret ->
        Format.fprintf ppf "@[<1>(<func>"; (* FIXME add a name to Ret ? *)
        pp_ret ppf ret;
        Format.fprintf ppf ")@]";

and pp_ret : type a. Format.formatter -> a ret -> unit =
fun ppf -> function
| Pure _ -> ()
| App (f, d, v) -> Format.fprintf ppf "@ %a" pp_term v; pp_ret ppf f

(* Substitutions *)

module Subst = struct
  module Vmap = Map.Make (Var)
  type binding = B : 'a var * 'a term -> binding
  type t = binding Vmap.t

  let empty = Vmap.empty
  let add var t s = Vmap.add (Var.V var) (B (var, t)) s
  let find : type a. a var -> t -> a term option =
  fun v s ->
    try
      let B (v', t) = Vmap.find (Var.V v) s in
      match teq v.tid v'.tid with None -> None | Some Teq -> Some t
    with Not_found -> None
end

let rec _term_value : type a. a term -> Subst.t -> a =
fun t s -> match t with
| Ret (d, t) -> _ret_value t s
| Var v ->
    match Subst.find v s with
    | Some t -> _term_value t s
    | None -> raise Exit

and _ret_value : type a. a ret -> Subst.t -> a =
fun r s -> match r with
| Pure v -> v
| App (f, _, v) -> (_ret_value f s) (_term_value v s)

let term_value t s = try Some (_term_value t s) with Exit -> None
let ret_value t s = try Some (_ret_value t s) with Exit -> None

let rec term_ret : type a. a term -> Subst.t -> (a dom * a ret) option =
fun t s -> match t with
| Ret (d, t) -> Some (d, t)
| Var v ->
    match Subst.find v s with
    | Some t -> term_ret t s
    | None -> None

(* Unification *)

let rec walk t s = match t with
| Var v -> (match Subst.find v s with None -> t | Some v -> walk v s)
| t -> t

let rec unify : type a. a term -> a term -> Subst.t -> Subst.t option =
(* FIXME not T.R. *)
fun t0 t1 s -> match walk t0 s, walk t1 s with
| Var v0, Var v1 when v0.id = v1.id -> Some s
| Var v, t | t, Var v -> Some (Subst.add v t s)
| Ret (d0, r0), Ret (d1, r1) ->
    if not (d0.Dom.tid == d1.Dom.tid) then None else
    match r0, r1 with
    | Pure v0, Pure v1 -> if d0.Dom.equal v0 v1 then Some s else None
    | App _, App _ -> unify_ret r0 r1 s
    | _, _ -> None

and unify_ret : type a. a ret -> a ret -> Subst.t -> Subst.t option =
fun r0 r1 s -> match r0, r1 with
| App (f0, d0, v0), App (f1, d1, v1) ->
    begin match teq d0.Dom.tid d1.Dom.tid with
    | None -> None
    | Some Teq ->
        match unify v0 v1 s with
        | None -> None
        | Some s -> unify_ret f0 f1 s
    end
| Pure _, Pure _ -> Some s (* must unify by semantics,
                              FIXME what about testing with == *)
| _, _ -> None

(* State *)

type state = { next_id : int; subst : Subst.t }
let empty = { next_id = 0; subst = Subst.empty }

(* Goals *)

type goal = state -> state Seq.t

let fail _ = Seq.empty
let succeed st = Seq.cons st Seq.empty

let ( = ) t0 t1 st = match unify t0 t1 st.subst with
| None -> Seq.empty
| Some subst -> succeed { st with subst }

let ( || ) g0 g1 st = Seq.mplus (g0 st) (g1 st)
let ( && ) g0 g1 st = Seq.bind (g0 st) g1
let delay gazy st = Seq.delay (lazy ((Lazy.force gazy) st))

let fresh lambda st =
  let var = var st.next_id in
  lambda var { st with next_id = st.next_id + 1 }

module Fresh = struct
  let v1 = fresh
  let v2 lambda st =
    let v1 = var (st.next_id    ) in
    let v2 = var (st.next_id + 1) in
    (lambda v1 v2) { st with next_id = st.next_id + 2 }

  let v3 lambda st =
    let v1 = var (st.next_id    ) in
    let v2 = var (st.next_id + 1) in
    let v3 = var (st.next_id + 2) in
    (lambda v1 v2 v3) { st with next_id = st.next_id + 3 }

  let v4 lambda st =
    let v1 = var (st.next_id    ) in
    let v2 = var (st.next_id + 1) in
    let v3 = var (st.next_id + 2) in
    let v4 = var (st.next_id + 3) in
    (lambda v1 v2 v3 v4) { st with next_id = st.next_id + 4 }

  let v5 lambda st =
    let v1 = var (st.next_id    ) in
    let v2 = var (st.next_id + 1) in
    let v3 = var (st.next_id + 2) in
    let v4 = var (st.next_id + 3) in
    let v5 = var (st.next_id + 4) in
    (lambda v1 v2 v3 v4 v5) { st with next_id = st.next_id + 5 }

  let v6 lambda st =
    let v1 = var (st.next_id    ) in
    let v2 = var (st.next_id + 1) in
    let v3 = var (st.next_id + 2) in
    let v4 = var (st.next_id + 3) in
    let v5 = var (st.next_id + 4) in
    let v6 = var (st.next_id + 5) in
    (lambda v1 v2 v3 v4 v5 v6) { st with next_id = st.next_id + 6 }
end

(* Reification *)

module Value = struct
  type 'a t = 'a term * Subst.t

  let v var subst = (var, subst)

  let name (var, _) = match var with
  | Var v -> Format.asprintf "%a" pp_var v | _ -> assert false

  let find (var, subst) = term_value var subst
  let get (var, subst) = match term_value var subst with
  | None -> invalid_arg (strf "%a is undefined" pp_term var)
  | Some v -> v

  let term (var, subst) = walk var subst

  let pp ppf (var, subst) = match term_ret var subst with
  | None -> pp_term ppf var
  | Some (d, ret) ->
      match ret_value ret subst with
      | Some v -> d.Dom.pp ppf v
      | None -> pp_ret ppf ret
end

type 'a value = 'a Value.t
type ('q, 'r) reifier = { next_id : int; query : 'q; reify : state -> 'r }

let reifier query reify = { next_id = 0; query; reify = (fun _ -> reify) }
let query ?name r =
  let var = var ?name r.next_id in
  let next_id = r.next_id + 1 in
  let query = r.query var in
  let reify st = r.reify st (Value.v var st.subst) in
  { next_id; query; reify }

let run r = Seq.map r.reify (r.query { empty with next_id = r.next_id })
let rec success g = not (Seq.is_empty (g empty))

module Query = struct
  let v1 ?n0 r = query ?name:n0 r
  let v2 ?n0 ?n1 r =
    let v0 = var ?name:n0 (r.next_id    ) in
    let v1 = var ?name:n1 (r.next_id + 1) in
    let next_id = r.next_id + 2 in
    let query = r.query v0 v1 in
    let reify st = r.reify st (Value.v v0 st.subst) (Value.v v1 st.subst) in
    { next_id; query; reify }

  let v3 ?n0 ?n1 ?n2 r =
    let v0 = var ?name:n0 (r.next_id    ) in
    let v1 = var ?name:n1 (r.next_id + 1) in
    let v2 = var ?name:n2 (r.next_id + 2) in
    let next_id = r.next_id + 3 in
    let query = r.query v0 v1 v2 in
    let reify st = r.reify st
        (Value.v v0 st.subst) (Value.v v1 st.subst) (Value.v v2 st.subst)
    in
    { next_id; query; reify }

  let v4 ?n0 ?n1 ?n2 ?n3 r =
    let v0 = var ?name:n0 (r.next_id    ) in
    let v1 = var ?name:n1 (r.next_id + 1) in
    let v2 = var ?name:n2 (r.next_id + 2) in
    let v3 = var ?name:n3 (r.next_id + 3) in
    let next_id = r.next_id + 4 in
    let query = r.query v0 v1 v2 v3 in
    let reify st = r.reify st
        (Value.v v0 st.subst) (Value.v v1 st.subst) (Value.v v2 st.subst)
        (Value.v v3 st.subst)
    in
    { next_id; query; reify }

  let v5 ?n0 ?n1 ?n2 ?n3 ?n4 r =
    let v0 = var ?name:n0 (r.next_id    ) in
    let v1 = var ?name:n1 (r.next_id + 1) in
    let v2 = var ?name:n2 (r.next_id + 2) in
    let v3 = var ?name:n3 (r.next_id + 3) in
    let v4 = var ?name:n4 (r.next_id + 4) in
    let next_id = r.next_id + 5 in
    let query = r.query v0 v1 v2 v3 v4 in
    let reify st = r.reify st
        (Value.v v0 st.subst) (Value.v v1 st.subst) (Value.v v2 st.subst)
        (Value.v v3 st.subst) (Value.v v4 st.subst)
    in
    { next_id; query; reify }

  let v6 ?n0 ?n1 ?n2 ?n3 ?n4 ?n5 r =
    let v0 = var ?name:n0 (r.next_id    ) in
    let v1 = var ?name:n1 (r.next_id + 1) in
    let v2 = var ?name:n2 (r.next_id + 2) in
    let v3 = var ?name:n3 (r.next_id + 3) in
    let v4 = var ?name:n4 (r.next_id + 4) in
    let v5 = var ?name:n5 (r.next_id + 5) in
    let next_id = r.next_id + 6 in
    let query = r.query v0 v1 v2 v3 v4 v5 in
    let reify st = r.reify st
        (Value.v v0 st.subst) (Value.v v1 st.subst) (Value.v v2 st.subst)
        (Value.v v3 st.subst) (Value.v v4 st.subst) (Value.v v5 st.subst)
    in
    { next_id; query; reify }
end



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
