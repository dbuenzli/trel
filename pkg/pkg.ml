#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "rel" @@ fun c ->
  Ok [ Pkg.mllib "src/rel.mllib";
       Pkg.mllib ~api:[] "src/rel_top.mllib";
       Pkg.lib "src/rel_top_init.ml";
       Pkg.test "test/test";
       Pkg.test "test/test_tree";
       Pkg.test "test/test_list";
       Pkg.test "test/mk_test";
       Pkg.test "test/mkv_test";
       Pkg.test "test/examples"; ]
