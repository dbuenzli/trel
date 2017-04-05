(*---------------------------------------------------------------------------
   Copyright (c) 2017 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let strf = Fmt.strf

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
  | Delay xs -> is_empty (Lazy.force xs)

  let rec head = function
  | Empty -> None
  | Cons (x, _) -> Some x
  | Delay xs -> head (Lazy.force xs)

  let rec get_head = function
  | Empty -> invalid_arg "Sequence is empty"
  | Cons (x, _) -> x
  | Delay xs -> get_head (Lazy.force xs)

  let rec tail = function
  | Empty -> invalid_arg "Sequence is empty"
  | Cons (_, xs) -> xs
  | Delay xs -> tail (Lazy.force xs)

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
        | Cons (x, xs) ->
            let limit = if limit = -1 then limit else limit - 1 in
            loop limit (x :: acc) xs
    in
    loop limit [] s

  let rec mplus s0 s1 = match s0 with
  | Empty -> s1
  | Cons (x, xs) -> Cons (x, Delay (lazy (mplus s1 xs)))
  | Delay xs ->
      match s1 with
      | Empty -> s0
      | Cons (y, ys) -> Cons (y, Delay (lazy (mplus (Lazy.force xs) ys)))
      | Delay ys -> Delay (lazy (mplus (Lazy.force xs) s1))

  let rec bind s f = match s with
  | Empty -> Empty
  | Cons (x, xs) -> mplus (f x) (Delay (lazy (bind xs f)))
  | Delay s -> Delay (lazy (bind (Lazy.force s) f))

  let rec map f s = match s with
  | Empty -> Empty
  | Cons (x, xs) -> Cons (f x, map f xs)
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

  let pp_abstr ppf _ = Fmt.pf ppf "<abstr>"

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

  let pp ppf d = Fmt.string ppf d.name

  (* Predefined domains *)

  let unit =
    let pp ppf () = Fmt.pf ppf "()" in
    v ~name:"unit" ~pp ~equal:(( = ) : unit -> unit -> bool) ()

  let bool =
    v ~name:"bool" ~pp:Fmt.bool ~equal:(( = ) : bool -> bool -> bool) ()

  let int =
    v ~name:"int" ~pp:Fmt.int ~equal:(( = ) : int -> int -> bool) ()

  let float =
    v ~name:"float" ~pp:Fmt.float ~equal:(( = ) : float -> float -> bool) ()

  let string =
    v ~name:"string" ~pp:Fmt.string ~equal:(( = ) : string -> string -> bool) ()

  let pair f s =
    let name = strf "%s * %s" f.name s.name in
    let pp = Fmt.Dump.pair f.pp s.pp in
    let equal (f0, s0) (f1, s1) = f.equal f0 f1 && s.equal s0 s1 in
    v ~name ~pp ~equal ()

  let list e =
    let equal l0 l1 = List.for_all2 e.equal l0 l1 in
    let pp = Fmt.Dump.list e.pp in
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
| Some n -> Fmt.pf ppf "%s" n
| None -> Fmt.pf ppf "_%d" v.id

let rec pp_term : type a. Format.formatter -> a term -> unit =
(* FIXME not T.R. *)
fun ppf -> function
| Var v -> pp_var ppf v
| Ret (d, ret) ->
    match ret with
    | Pure v -> d.Dom.pp ppf v
    | App _ as ret -> pp_ret ppf ret (* FIXME add a name to Ret ? *)

and pp_ret : type a. Format.formatter -> a ret -> unit =
fun ppf -> function
| Pure _ -> ()
| App (f, d, v) ->
    Fmt.pf ppf "@[<1>(<fun>";
    Fmt.pf ppf "@ %a %a" pp_term v pp_ret f;
    Fmt.pf ppf ")@]";

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

  let iter_bindings f = Vmap.iter (fun _ v -> f v)
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

let pp_var_value pp_value v ppf var =
  Fmt.pf ppf "@[<1>(%a@ = %a)@]" pp_var var pp_value v

let pp_binding ppf (Subst.B (var, term)) =
  pp_var_value pp_term term ppf var

let pp_binding_value subst ppf (Subst.B (var, term) as b) =
  match term_ret term subst with
  | None -> pp_binding ppf b
  | Some (d, ret) ->
      match ret_value ret subst with
      | Some v -> pp_var_value d.Dom.pp v ppf var
      | None -> pp_var_value pp_ret ret ppf var

let pp_subst ppf subst =
  Fmt.pf ppf "@[<1>(%a)@]"
    (Fmt.iter ~sep:Fmt.sp Subst.iter_bindings pp_binding) subst

let pp_subst_values ppf subst =
  Fmt.pf ppf "@[<1>(%a)@]"
    (Fmt.iter ~sep:Fmt.sp Subst.iter_bindings (pp_binding_value subst)) subst

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
| Pure f0, Pure f1 when f0 == f1 -> Some s
| _, _ -> None

(* State *)

type state = { next_vid : int; subst : Subst.t }
let empty = { next_vid = 0; subst = Subst.empty }

let pp_state ppf st = pp_subst_values ppf st.subst

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
  let var = var st.next_vid in
  lambda var { st with next_vid = st.next_vid + 1 }

module Fresh = struct
  let v1 = fresh
  let v2 lambda st =
    let v1 = var (st.next_vid    ) in
    let v2 = var (st.next_vid + 1) in
    (lambda v1 v2) { st with next_vid = st.next_vid + 2 }

  let v3 lambda st =
    let v1 = var (st.next_vid    ) in
    let v2 = var (st.next_vid + 1) in
    let v3 = var (st.next_vid + 2) in
    (lambda v1 v2 v3) { st with next_vid = st.next_vid + 3 }

  let v4 lambda st =
    let v1 = var (st.next_vid    ) in
    let v2 = var (st.next_vid + 1) in
    let v3 = var (st.next_vid + 2) in
    let v4 = var (st.next_vid + 3) in
    (lambda v1 v2 v3 v4) { st with next_vid = st.next_vid + 4 }

  let v5 lambda st =
    let v1 = var (st.next_vid    ) in
    let v2 = var (st.next_vid + 1) in
    let v3 = var (st.next_vid + 2) in
    let v4 = var (st.next_vid + 3) in
    let v5 = var (st.next_vid + 4) in
    (lambda v1 v2 v3 v4 v5) { st with next_vid = st.next_vid + 5 }

  let v6 lambda st =
    let v1 = var (st.next_vid    ) in
    let v2 = var (st.next_vid + 1) in
    let v3 = var (st.next_vid + 2) in
    let v4 = var (st.next_vid + 3) in
    let v5 = var (st.next_vid + 4) in
    let v6 = var (st.next_vid + 5) in
    (lambda v1 v2 v3 v4 v5 v6) { st with next_vid = st.next_vid + 6 }
end

(* Reification *)

module Value = struct
  type 'a t = 'a term * Subst.t

  let v var subst = (var, subst)

  let name (var, _) = match var with
  | Var v -> strf "%a" pp_var v | _ -> assert false

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

  let get1 x = get x
  let get2 x y = get x, get y
  let get3 x y z = get x, get y, get z
  let get4 x y z r = get x, get y, get z, get r
  let get5 x y z r s = get x, get y, get z, get r, get s
  let get6 x y z r s t = get x, get y, get z, get r, get s, get t
  let find1 x = find x
  let find2 x y = find x, find y
  let find3 x y z = find x, find y, find z
  let find4 x y z r = find x, find y, find z, find r
  let find5 x y z r s = find x, find y, find z, find r, find s
  let find6 x y z r s t = find x, find y, find z, find r, find s, find t
end

type 'a value = 'a Value.t
type ('q, 'r) reifier = { next_vid : int; query : 'q; reify : state -> 'r }

let reifier query reify = { next_vid = 0; query; reify = (fun _ -> reify) }
let query ?name r =
  let var = var ?name r.next_vid in
  let next_vid = r.next_vid + 1 in
  let query = r.query var in
  let reify st = r.reify st (Value.v var st.subst) in
  { next_vid; query; reify }

let _run r = r.query { empty with next_vid = r.next_vid }
let run r = Seq.map r.reify (_run r)
let rec success g = not (Seq.is_empty (g empty))

module Query = struct
  let v1 ?n0 r = query ?name:n0 r
  let v2 ?n0 ?n1 r =
    let v0 = var ?name:n0 (r.next_vid    ) in
    let v1 = var ?name:n1 (r.next_vid + 1) in
    let next_vid = r.next_vid + 2 in
    let query = r.query v0 v1 in
    let reify st = r.reify st (Value.v v0 st.subst) (Value.v v1 st.subst) in
    { next_vid; query; reify }

  let v3 ?n0 ?n1 ?n2 r =
    let v0 = var ?name:n0 (r.next_vid    ) in
    let v1 = var ?name:n1 (r.next_vid + 1) in
    let v2 = var ?name:n2 (r.next_vid + 2) in
    let next_vid = r.next_vid + 3 in
    let query = r.query v0 v1 v2 in
    let reify st = r.reify st
        (Value.v v0 st.subst) (Value.v v1 st.subst) (Value.v v2 st.subst)
    in
    { next_vid; query; reify }

  let v4 ?n0 ?n1 ?n2 ?n3 r =
    let v0 = var ?name:n0 (r.next_vid    ) in
    let v1 = var ?name:n1 (r.next_vid + 1) in
    let v2 = var ?name:n2 (r.next_vid + 2) in
    let v3 = var ?name:n3 (r.next_vid + 3) in
    let next_vid = r.next_vid + 4 in
    let query = r.query v0 v1 v2 v3 in
    let reify st = r.reify st
        (Value.v v0 st.subst) (Value.v v1 st.subst) (Value.v v2 st.subst)
        (Value.v v3 st.subst)
    in
    { next_vid; query; reify }

  let v5 ?n0 ?n1 ?n2 ?n3 ?n4 r =
    let v0 = var ?name:n0 (r.next_vid    ) in
    let v1 = var ?name:n1 (r.next_vid + 1) in
    let v2 = var ?name:n2 (r.next_vid + 2) in
    let v3 = var ?name:n3 (r.next_vid + 3) in
    let v4 = var ?name:n4 (r.next_vid + 4) in
    let next_vid = r.next_vid + 5 in
    let query = r.query v0 v1 v2 v3 v4 in
    let reify st = r.reify st
        (Value.v v0 st.subst) (Value.v v1 st.subst) (Value.v v2 st.subst)
        (Value.v v3 st.subst) (Value.v v4 st.subst)
    in
    { next_vid; query; reify }

  let v6 ?n0 ?n1 ?n2 ?n3 ?n4 ?n5 r =
    let v0 = var ?name:n0 (r.next_vid    ) in
    let v1 = var ?name:n1 (r.next_vid + 1) in
    let v2 = var ?name:n2 (r.next_vid + 2) in
    let v3 = var ?name:n3 (r.next_vid + 3) in
    let v4 = var ?name:n4 (r.next_vid + 4) in
    let v5 = var ?name:n5 (r.next_vid + 5) in
    let next_vid = r.next_vid + 6 in
    let query = r.query v0 v1 v2 v3 v4 v5 in
    let reify st = r.reify st
        (Value.v v0 st.subst) (Value.v v1 st.subst) (Value.v v2 st.subst)
        (Value.v v3 st.subst) (Value.v v4 st.subst) (Value.v v5 st.subst)
    in
    { next_vid; query; reify }
end

module Reifier = struct

  let get1 ?n0 q = Query.v1 ?n0 (reifier q Value.get1)
  let get2 ?n0 ?n1 q = Query.v2 ?n0 ?n1 (reifier q Value.get2)
  let get3 ?n0 ?n1 ?n2 q = Query.v3 ?n0 ?n1 ?n2 (reifier q Value.get3)
  let get4 ?n0 ?n1 ?n2 ?n3 q = Query.v4 ?n0 ?n1 ?n2 ?n3 (reifier q Value.get4)
  let get5 ?n0 ?n1 ?n2 ?n3 ?n4 q =
    Query.v5 ?n0 ?n1 ?n2 ?n3 ?n4 (reifier q Value.get5)

  let get6 ?n0 ?n1 ?n2 ?n3 ?n4 ?n5 q =
    Query.v6 ?n0 ?n1 ?n2 ?n3 ?n4 ?n5 (reifier q Value.get6)

  let find1 ?n0 q = Query.v1 ?n0 (reifier q Value.find1)
  let find2 ?n0 ?n1 q = Query.v2 ?n0 ?n1 (reifier q Value.find2)
  let find3 ?n0 ?n1 ?n2 q = Query.v3 ?n0 ?n1 ?n2 (reifier q Value.find3)
  let find4 ?n0 ?n1 ?n2 ?n3 q = Query.v4 ?n0 ?n1 ?n2 ?n3 (reifier q Value.find4)
  let find5 ?n0 ?n1 ?n2 ?n3 ?n4 q =
    Query.v5 ?n0 ?n1 ?n2 ?n3 ?n4 (reifier q Value.find5)

  let find6 ?n0 ?n1 ?n2 ?n3 ?n4 ?n5 q =
    Query.v6 ?n0 ?n1 ?n2 ?n3 ?n4 ?n5 (reifier q Value.find6)
end

module Run = struct
  let get1 ?limit q = Seq.to_list ?limit @@ run (Reifier.get1 q)
  let get2 ?limit q = Seq.to_list ?limit @@ run (Reifier.get2 q)
  let get3 ?limit q = Seq.to_list ?limit @@ run (Reifier.get3 q)
  let get4 ?limit q = Seq.to_list ?limit @@ run (Reifier.get4 q)
  let get5 ?limit q = Seq.to_list ?limit @@ run (Reifier.get5 q)
  let get6 ?limit q = Seq.to_list ?limit @@ run (Reifier.get6 q)
  let find1 ?limit q = Seq.to_list ?limit @@ run (Reifier.find1 q)
  let find2 ?limit q = Seq.to_list ?limit @@ run (Reifier.find2 q)
  let find3 ?limit q = Seq.to_list ?limit @@ run (Reifier.find3 q)
  let find4 ?limit q = Seq.to_list ?limit @@ run (Reifier.find4 q)
  let find5 ?limit q = Seq.to_list ?limit @@ run (Reifier.find5 q)
  let find6 ?limit q = Seq.to_list ?limit @@ run (Reifier.find6 q)
end

(* State introspection *)

let states r = _run r
let inspect ?limit r = Seq.to_list ?limit (states r)

module Inspect = struct
  let v1 ?limit ?n0 q = inspect ?limit (Reifier.find1 ?n0 q)
  let v2 ?limit ?n0 ?n1 q = inspect ?limit (Reifier.find2 ?n0 ?n1 q)
  let v3 ?limit ?n0 ?n1 ?n2 q = inspect ?limit (Reifier.find3 ?n0 ?n1 ?n2 q)
  let v4 ?limit ?n0 ?n1 ?n2 ?n3 q =
    inspect ?limit (Reifier.find4 ?n0 ?n1 ?n2 ?n3 q)

  let v5 ?limit ?n0 ?n1 ?n2 ?n3 ?n4 q =
    inspect ?limit (Reifier.find5 ?n0 ?n1 ?n2 ?n3 ?n4 q)

  let v6 ?limit ?n0 ?n1 ?n2 ?n3 ?n4 ?n5 q =
    inspect ?limit (Reifier.find6 ?n0 ?n1 ?n2 ?n3 ?n4 ?n5 q)
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
