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
open Printf
open Base

let check_eq msg expected actual =
  assert_equal expected actual ?msg:(Some msg)

let atop = Atom.to_ptr
let ptoa = Atom.from_ptr
let atos = Atom.to_string
let stoa = Atom.from_string

external atos2 : Atom.t -> string = "caml_hc_extract_string"

let perr s =
  begin
    output_string stderr s;
    flush stderr
  end

let check_ptr_eq expected actual =
  let message = "Pointer mismatch: " ^ (show_ptr (expected)) ^ " vs " ^ (show_ptr (actual))
  in check_eq message expected actual


let test_atoms_match () =
  let atom1 = Atom.from_string "hello, world" in
  let atom2 = Atom.from_string "hello, world"
  in check_eq "mismatch" atom1 atom2

let test_atoms_match2 () =
  let s = "hello, world"
  in begin
    String.set s 0 'X';
    check_eq "mismatch" (Atom.from_string "Xello, world") (Atom.from_string s)
  end

let test_atom_pointers_match () =
  let s0 = (Atom.to_ptr (Atom.from_string "hello, world")) in
  let s1 = (Atom.to_ptr (Atom.from_string "hello, world"))
  in (check_ptr_eq s0 s1)

let test_atom_pointers_match2 () =
  let s = "hello, world"
  in begin
    String.set s 0 'Y';
    check_ptr_eq (Atom.to_ptr (Atom.from_string "Yello, world")) (Atom.to_ptr (stoa s))
  end

let test_back_and_forth () =
  let original_s = "And now for something completely different." in
  let hashconsed = stoa (original_s) in
  let () = perr "And now we're going back...\n" in
  let s = atos2 (hashconsed) in
  let () = perr "Good.  Now here's the string...\n" in
  let () = perr s in
  let () = perr "\nTime to check...\n"
  in check_eq ("string("^s^")") (original_s) (s)

let _ = Gc.create_alarm (function () -> Gc.print_stat (stderr))

let all_tests = "native" >:::
  [
    "atoms-match" >:: test_atoms_match;
    "atoms-match2" >:: test_atoms_match2;
    "atom-pointers-match" >:: test_atom_pointers_match;
    "atom-pointers-match2" >:: test_atom_pointers_match2;
    "back-and-forth" >:: test_back_and_forth;
  ]

let _ = run_test_tt_main (all_tests)
