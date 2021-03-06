(***************************************************************************
 This file is Copyright (C) 2012 Christoph Reichenbach

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the
   Free Software Foundation, Inc.
   59 Temple Place, Suite 330
   Boston, MA  02111-1307
   USA

 The author can be reached as "creichen" at the usual gmail domain.

***************************************************************************)

open OUnit
open Strlib

let check_eq s0 s1 () =
  assert_equal s0 s1 ?msg:(Some ("Expected `"^s0^"' but got `"^s1^"'"))

let all_tests = "strlib" >:::
  [
    "strip-whitespace-0" >:: check_eq  "foo, bar" (strip_whitespace "  foo, bar ");
    "strip-whitespace-1" >:: check_eq  "a" (strip_whitespace "  a");
    "strip-whitespace-2" >:: check_eq  "a" (strip_whitespace "a  ");
    "dequote-0" >:: check_eq "" (dequote "");
    "dequote-1" >:: check_eq "a" (dequote "a");
    "dequote-2" >:: check_eq "\".x" (dequote "\\\"\\.x");
    "dequote-3" >:: check_eq "foo\\bar" (dequote "foo\\\\bar");
    "dequote-4" >:: check_eq "" (dequote "\\");
  ]


let _ = run_test_tt_main (all_tests)

