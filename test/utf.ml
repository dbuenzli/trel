(*---------------------------------------------------------------------------
   Copyright (c) 2017 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)


(* https://github.com/ocaml/ocaml/pull/1091 *)

let utf_8_spec =
  (* UTF-8 byte sequences, cf. table 3.7 Unicode 9. *)
  [(0x0000,0x007F),     [|(0x00,0x7F)|];
   (0x0080,0x07FF),     [|(0xC2,0xDF); (0x80,0xBF)|];
   (0x0800,0x0FFF),     [|(0xE0,0xE0); (0xA0,0xBF); (0x80,0xBF)|];
   (0x1000,0xCFFF),     [|(0xE1,0xEC); (0x80,0xBF); (0x80,0xBF)|];
   (0xD000,0xD7FF),     [|(0xED,0xED); (0x80,0x9F); (0x80,0xBF)|];
   (0xE000,0xFFFF),     [|(0xEE,0xEF); (0x80,0xBF); (0x80,0xBF)|];
   (0x10000,0x3FFFF),   [|(0xF0,0xF0); (0x90,0xBF); (0x80,0xBF); (0x80,0xBF)|];
   (0x40000,0xFFFFF),   [|(0xF1,0xF3); (0x80,0xBF); (0x80,0xBF); (0x80,0xBF)|];
   (0x100000,0x10FFFF), [|(0xF4,0xF4); (0x80,0x8F); (0x80,0xBF); (0x80,0xBF)|]]
;;

let utf_16be_spec =
  (* UTF-16BE byte sequences, derived from table 3.5 Unicode 9. *)
  [(0x0000,0xD7FF), [|(0x00,0xD7); (0x00,0xFF)|];
   (0xE000,0xFFFF), [|(0xE0,0xFF); (0x00,0xFF)|];
   (0x10000,0x10FFFF), [|(0xD8,0xDB); (0x00,0xFF); (0xDC,0xDF); (0x00,0xFF)|]]
;;


let udom = Rel.Dom.v ~equal:Uchar.equal ()

let





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
