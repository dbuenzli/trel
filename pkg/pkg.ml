#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "trel" @@ fun c ->
  Ok [ Pkg.mllib "src/trel.mllib";
       Pkg.mllib ~api:[] "src/trel_top.mllib";
       Pkg.lib "src/trel_top_init.ml";
       Pkg.bin "test/reliza";
       Pkg.bin "test/sreliza";
       Pkg.test "test/test";
       Pkg.test "test/test_tree";
       Pkg.test "test/test_list";
       Pkg.test "test/mk_test";
       Pkg.test "test/mkv_test";
       Pkg.test "test/examples"; ]
