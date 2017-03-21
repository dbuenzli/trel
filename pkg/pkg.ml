#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "rel" @@ fun c ->
  Ok [ Pkg.mllib "src/rel.mllib";
       Pkg.test "test/test"; ]
