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

open Base
open OUnit

module SimpleTable = Simple_table
module CombinedTable = Combined_table

module LabelAdder =
  functor (M: sig
    type t
    val name : string
    val create : int -> t
    val insert : t -> Tuple.t -> unit
    val contains : t -> Tuple.t -> bool
    val bind_all : t -> Literal.body_t -> Env.t -> (Env.t -> unit) -> unit
    val show : t -> string
    val drop : t -> unit
  end) ->
struct
  type t = M.t
  let name = M.name
  let create = M.create

  let labelled_insert = M.insert
  let labelled_contains = M.contains

  let labelled_bind_all = M.bind_all
  let bind_all = M.bind_all
  let show = M.show

  let label_expand_array = Tuple.positional

  let insert t b = M.insert t (label_expand_array b)
  let contains t b = M.contains t (label_expand_array b)
  let drop = M.drop
end

module CTable = LabelAdder(CombinedTable)
module STable = LabelAdder(SimpleTable)
(*module NTable = LabelAdder(NativeTable)*)

let check_eq msg expected actual =
  assert_equal expected actual ?msg:(Some msg)

let afs = Atom.from_string
let tafs = Array.map Atom.from_string
let tpafs t = Tuple.positional (Array.map Atom.from_string t)

module BasicTester =
  functor (Table: sig
    type t
    val create : int -> t
    val drop : t -> unit
    val labelled_insert : t -> Tuple.t -> unit
    val labelled_contains : t -> Tuple.t -> bool
    val labelled_bind_all : t -> Literal.body_t -> Env.t -> (Env.t -> unit) -> unit
    val name : string
  end) -> 
struct

  let test_simple_basic () =
    let table = Table.create (2)
    in begin
      check_eq "empty-0" false  (Table.labelled_contains table (tpafs [|"foo"; "bar"|]));
      Table.labelled_insert table (tpafs [|"foo"; "bar"|]);
      Table.labelled_insert table (tpafs [|"foo"; "baz"|]);
      check_eq "nonempty-0" true  (Table.labelled_contains table (tpafs [|"foo"; "bar"|]));
      check_eq "nonempty-1" true  (Table.labelled_contains table (tpafs [|"foo"; "baz"|]));
      check_eq "empty-1" false  (Table.labelled_contains table (tpafs [|"foo"; "quux"|]));
      Table.drop table
    end

  let test_simple_bind () =
    let table = Table.create (3) in
    let results : tuple list ref = ref [] in
    let clear_results () = results := [] in
    let cont (_, vars) (env) = results := (Array.map (function _ -> Label.none) vars, Array.map (Env.find env) vars) :: (!results) in
    let check_result message (expected : tuple list) (actual : tuple list) =
      let expected = Tuple.sort expected in
      let actual = Tuple.sort actual in
      let show_tuples atoms = "[" ^ (String.concat "; " (List.map Tuple.show atoms)) ^ "]"
      in (check_eq
	    ("Mismatch in " ^ message ^ "\nexpected:\t" ^ (show_tuples expected) ^ "\nactual:  \t" ^ (show_tuples actual))
	    expected actual)
    in let env = Env.fresh ()
       in let check_query (message) (bindings) (query) (result_vars) (expected_result) =
	    let do_bind (name, value) = Env.bind env name (afs value)
	    in begin
	      clear_results ();
	      Env.clear env;
	      List.iter do_bind bindings;
	      Table.labelled_bind_all table (Tuple.positional query) env (cont (Tuple.positional result_vars));
	      check_result message (List.map tpafs expected_result)  (!results);
	    end
	  in let insert_table t = Table.labelled_insert table (tpafs t)
	  in begin
	    insert_table [|"foo"; "1"; "bar"|];
	    insert_table [|"foo"; "1"; "baz"|];
	    insert_table [|"glorb"; "2"; "2"|];
	    insert_table [|"quux"; "1"; "baz"|];
	    insert_table [|"quux"; "1"; "1"|];

	    check_query "simple"   [("X", "foo")]        [|"X"; "Y"; "Z"|]  [|"Z"|]  [[|"bar"|]; [|"baz"|]];
	    check_query "empty"    [("X", "not-found")]  [|"X"; "Y"; "Z"|]  [|"Z"|]  [];
	    check_query "eq-bind"  []                    [|"X"; "Y"; "Y"|]  [|"X"|]  [[|"glorb"|]; [|"quux"|]];
	    List.iter (function var -> check_eq ("env-clear-" ^ var) None (Env.lookup env var)) ["Y"; "Z"];
	    check_query "check-1"    [("X", "foo"); ("Y", "1"); ("Z", "bar")]        [|"X"; "Y"; "Z"|]  [|"Z"|]  [[|"bar"|]];
	    check_query "check-0"    [("X", "foo"); ("Y", "1"); ("Z", "unfound")]    [|"X"; "Y"; "Z"|]  [|"Z"|]  [];
	    Table.drop table
	  end
end

module SimpleTableTester = BasicTester(STable)
(*module NativeTableTester = BasicTester(NTable)*)


let test_combined_table_delta_and_flag () =
  let base_table = STable.create (7) in
  let delta_table = STable.create (7) in
  let flag = ref false in
  let join_table = CombinedTable.create_delta (flag, delta_table, base_table) in
  begin
    STable.insert base_table (tafs [|"foo"|]);
    check_eq "flag-initially-false" false (!flag);
    CTable.insert join_table (tafs [|"foo"|]);
    check_eq "flag-not-updated" false (!flag);
    check_eq "redundant-update-not-in-delta" false (STable.contains delta_table (tafs [|"foo"|]));
    CTable.insert join_table (tafs [|"bar"|]);
    check_eq "flag-updated" true (!flag);
    check_eq ("real-update-in-delta(" ^ (STable.show delta_table) ^ ")") true (STable.contains delta_table (tafs [|"bar"|]));
    check_eq "real-update-in-base" true (STable.contains base_table (tafs [|"bar"|]));
  end


let test_combined_table_replace_delta () =
  let base_table = STable.create (7) in
  let delta_foo_table = STable.create (7) in
  let delta_bar_table = STable.create (7) in
  let delta_quux_table = STable.create (7) in
  let flag = ref false in
  let join_table = CombinedTable.create_delta (flag, delta_foo_table, base_table) in
  let all_values = ["foo"; "bar"; "quux"] in
  let check_containment tablename table selection =
    let contains s = List.exists (function n -> n = s) selection in
    let try_value v =
      let cs = if contains v
	then "assert-contains"
	else "assert-does-not-contain"
      in check_eq (tablename ^ "(" ^ (STable.show table) ^ ") " ^ cs ^ " " ^ v) (contains v) (STable.contains table (tafs [|v|]))
    in List.iter try_value all_values
  in
  begin
    CTable.insert join_table (tafs [|"foo"|]);
    CombinedTable.update_delta join_table delta_bar_table;
    CTable.insert join_table (tafs [|"bar"|]);
    CombinedTable.update_delta join_table delta_quux_table;
    CTable.insert join_table (tafs [|"quux"|]);
    check_containment "base" base_table all_values;
    check_containment "delta-foo-table" delta_foo_table ["foo"];
    check_containment "delta-bar-table" delta_bar_table ["bar"];
    check_containment "delta-quux-table" delta_quux_table ["quux"];
  end
  

let all_tests = "table" >:::
  [
    "simple-basic" >:: SimpleTableTester.test_simple_basic;
    "simple-bind" >:: SimpleTableTester.test_simple_bind;
(*    "native-basic" >:: NativeTableTester.test_simple_basic;*)
(*    "native-bind" >:: NativeTableTester.test_simple_bind;*)
    "combined-update" >:: test_combined_table_delta_and_flag;
    "combined-new-delta" >:: test_combined_table_replace_delta;
  ]

let _ = run_test_tt_main (all_tests)
