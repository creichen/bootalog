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

 The author can be reached as "reichenb" at "colorado.edu".

***************************************************************************)

open Base
open Frontend
open Program.Lexeme
open OUnit
open Error_test_helper
module AP = Access_path
module PI = Primop_interface

let check_eq_s (actual) (actual_str) (expected) (expected_str) =
  assert_equal actual expected ?msg:(Some (Printf.sprintf "Mismatch:\nactual  : %s\nexpected: %s" (actual_str) (expected_str)))

let check_eq (show) (actual) (expected) =
  check_eq_s actual (show actual) expected (show expected)

let show_int i = Printf.sprintf "%d" i


module AccessMode =
  struct
    open PI
    open Primops

    let dummy_fn {get=_; set=_; cont=_} = ()

    let amode (modelist) (modecost) = mode (modelist) (cost modecost) (dummy_fn)

    let check (var_mode_list) (access_mode_list) (expected : access_mode list) () =
      let show_multi alists = String.concat "; " (List.map show_access_mode (alists)) in
      let result = access_modes (var_mode_list) (access_mode_list) in
      let checkarg = Some (Printf.sprintf "Mismatch:\nactual  : %s\nexpected: %s" (show_multi result) (show_multi expected)) in
      let check getter = assert_equal (List.map getter result) (List.map getter expected) ?msg:checkarg
      in begin
	check (function a -> a.variable_modes);
	check (function a -> a.cost);
      end

    let test_simple_0 = check [bb] [] ([])
    let test_simple_1 = check [bb] [amode [bb] 3] ([amode [bb] 3])
    let test_simple_2 = check [ff] [amode [bb] 3] ([])
    let test_simple_3 = check [bb] [amode [bb] 3; amode [ff] 2] ([amode [bb] 3])
    let test_simple_4 = check [bb] [amode [ff] 2; amode [bb] 3] ([amode [bb] 3])

    let long_modes = [amode [bb; bb] 3; amode [ff; ff] 4; amode [ff; bb] 5; amode [bb; ff] 6]

    let test_long_0 = check [bb; ff]
                            long_modes
			    ([amode [bb; ff] 6])
    let test_long_1 = check [ff; bb]
                            long_modes
			    ([amode [ff; bb] 5])
    let test_long_2 = check [ff; ff]
                            long_modes
			    ([amode [ff; ff] 4])

    let test_multi_0 = check [bb; ff]
                             [amode [bb; bb] 2; amode [bb; ff] 3; amode [ff; bb] 4; amode [bb; ff] 5; amode [ff; ff] 6]
			     ([amode [bb; ff] 3; amode [bb; ff] 5])

    let test_wildcards_0 = check [bb; bb; bb]
                                 [amode [Any; bb; ff] 0; amode [Any; ff; Any] 1; amode [Any; Any; Any] 2; amode [ff; Any; Any] 3]
				 ([amode [Any; Any; Any] 2])
    let test_wildcards_1 = check [bb; bb; bb]
                                 [amode [Any; bb; Any] 0; amode [Any; ff; Any] 1; amode [Any; Any; Any] 2; amode [ff; Any; Any] 3; amode [bb; bb; bb] 4]
				 ([amode [Any; bb; Any] 0; amode [Any; Any; Any] 2; amode [bb; bb; bb] 4])

    let test_fail_0 = check [bb; ff; bb]
                            [amode [Any; bb; Any] 0; amode [ff; ff; Any] 1; amode [Any; Any; ff] 2; amode [ff; Any; Any] 3; amode [bb; bb; bb] 4]
			    ([])
  end

module Adapter =
  struct
    open PI
    open Primops

    let show_list l = "[" ^ (String.concat ", " l) ^ "]"

    let test_cont () =
      let count = ref 0 in
      let body { get=_; set=_; cont } =
	begin
	  cont ();
	  cont ();
	  cont ();
	end in
      let env = Env.fresh() in
      let mcont (_) = count := 1 + !count
      in begin
	adapter (body) [||] env mcont;
	check_eq show_int 3 (!count);
      end

    let test_set () =
      let result = ref [] in
      let body { get=_; set; cont } =
	begin
	  set 1 "1a";
	  cont ();
	  set 0 "0b";
	  set 1 "1b";
	  cont ();
	end in
      let env = Env.fresh() in
      let mcont (env) = result := Env.find env "a" :: Env.find env "b" :: !result
      in begin
	Env.bind env "a" "0";
	adapter (body) [| "a"; "b" |] env mcont;
	(* list has the events in reverse order, due to prepending *)
	check_eq show_list ["0b"; "1b"; "0"; "1a"] (!result);
      end

    let test_get () =
      let result = ref 0 in
      let body { get; set=_; cont } =
	begin
	  if get 0 = "yes"
	  then cont ();
	end in
      let env = Env.fresh() in
      let mcont (_) = result := 1;
      in begin
	result := 0;
	Env.bind env "a" "no";
	adapter (body) [| "a" |] env mcont;
	check_eq show_int 0 (!result);

	result := 0;
	Env.bind env "a" "yes";
	adapter (body) [| "a" |] env mcont;
	check_eq show_int 1 (!result);
      end
  end

module Link =
  struct
    open Predicate
    open Literal

    let check_link_result actual expected =
      let show_link_result result =
	match result with
	  None			-> "(none)"
	| Some (l, None)	-> Printf.sprintf "(%s, --)" (Literal.show l)
	| Some (l, Some am)	-> Printf.sprintf "(%s, %s)" (Literal.show l) (PI.show_access_mode am)
      in match (actual, expected) with
	None, None		-> ()
      | Some (l0, None),
	  Some (l1, None)	-> check_eq_s (compare l0 l1) (show_link_result actual) 0 (show_link_result expected)
      | Some (l0, Some am0),
	  Some (l1, Some am1)	-> begin
	    check_eq_s (compare l0 l1) (show_link_result actual) 0 (show_link_result expected);
	    check_eq_s (am0.PI.variable_modes) (show_link_result actual) (am1.PI.variable_modes) (show_link_result expected);
	    check_eq_s (am0.PI.cost) (show_link_result actual) (am1.PI.cost) (show_link_result expected);
	  end
      | _			-> check_eq_s 1 (show_link_result actual) 0 (show_link_result expected) (* always fail *)

    let test (varlist) (literal) (expected_result) () =
      check_link_result (link (VarSet.of_list varlist) (literal)) (expected_result)

    let dummy_fn _ _ _ = ()
    let eval_dummy_fn = AccessMode.dummy_fn

    let eq = Primops.Sys.eq
    let leq = Predicate.Linked ("=[??]", value_of (PI.primop_id eq), dummy_fn)

    open Primops

    let test_p0 = test ["X"] (P "a", [|"Y"|]) (Some ((P "a", [|"Y"|]), None))
    let test_eq_0 = test ["X"] (eq, [|"X"; "Y"|]) (Some ((leq, [|"X"; "Y"|]), Some (mode [bb; ff] (write_cost 1) eval_dummy_fn)))
    let test_eq_1 = test ["Y"] (eq, [|"X"; "Y"|]) (Some ((leq, [|"X"; "Y"|]), Some (mode [ff; bb] (write_cost 1) eval_dummy_fn)))
    let test_eq_2 = test ["Y"; "X"] (eq, [|"X"; "Y"|]) (Some ((leq, [|"X"; "Y"|]), Some (mode [bb; bb] (min_cost) eval_dummy_fn)))
    let test_eq_fail = test ["Z"] (eq, [|"X"; "Y"|]) (None)
  end

module Expensive =
  struct
    open Predicate
    open Literal

    let check_cost vars literal expected_cost =
      check_eq PI.show_cost (estimate_access_cost (VarSet.of_list vars) literal) expected_cost

    let test_p_check () =
      check_cost ["X"; "Y"] (P "a", [|"X"; "Y"|]) (PI.cost Literal.predicate_element_check_cost)

    let test_p_contains_0 () =
      check_cost ["X"] (P "a", [|"X"; "Y"|]) (PI.cost (Literal.predicate_element_read_cost * Literal.base_predicate_entries))

    let test_p_contains_1 () =
      check_cost ["Y"] (P "a", [|"X"; "Y"|]) (PI.cost (Literal.predicate_element_read_cost * Literal.base_predicate_entries))

    let test_p_contains_2 () =
      check_cost ["Z"] (P "a", [|"X"; "Y"|]) (PI.cost (Literal.predicate_element_read_cost * Literal.base_predicate_entries))

    let test_delta_contains () =
      check_cost ["X"] (Delta "a", [|"X"; "Y"|]) (PI.cost (Literal.predicate_element_read_cost * Literal.delta_predicate_entries))

    open Primops

    let test_primop_eq_0 () =
      check_cost ["X"] (Link.eq, [|"X"; "Y"|]) (write_cost 1)

    let test_primop_eq_1 () =
      check_cost ["Y"] (Link.eq, [|"X"; "Y"|]) (write_cost 1)

    let test_primop_eq_2 () =
      check_cost ["X"; "Y"] (Link.eq, [|"X"; "Y"|]) (min_cost)

  end

module APath =
  struct
    let atom arg = (Predicate.atom, [|arg|])
    let p arg = (Predicate.P "p", [|arg|])
    let q (x, y) = (Predicate.P "q", [|x; y|])
    let r (x, y) = (Predicate.P "r", [|x; y|])
    let delta_r (x, y) = (Predicate.Delta "r", [|x; y|])
    let eq (x, y) = (Primops.Sys.eq, [|x; y|])
    let add (x, y, z) = (Primops.Sys.add, [|x; y; z|])
    let concat (x, y, z) = (Primops.Sys.concat, [|x; y; z|])
    let leq modestr (x, y) = (Predicate.Linked ("=["^modestr^"]", value_of (PI.primop_id Primops.Sys.eq), function _ -> function _ -> function _ -> ()),
			      [|x; y|])
    let ladd modestr (x, y, z) =
      (Predicate.Linked ("sys-add["^modestr^"]", value_of (PI.primop_id Primops.Sys.add), function _ -> function _ -> function _ -> ()),
       [|x; y; z|])
    let lconcat modestr (x, y, z) =
      (Predicate.Linked ("sys-concat["^modestr^"]", value_of (PI.primop_id Primops.Sys.concat), function _ -> function _ -> function _ -> ()),
       [|x; y; z|])
    let assign (var, atom) = (Predicate.Assign atom, [|var|])

    let show_tail tail =
      "[" ^ (String.concat ", " (List.map Literal.show tail)) ^ "]"

    let test_fail tail () =
      let ha0 = "X" in
      let ha1 = "X" in
      try
	begin
	  let (_, result) = AP.select (q(ha0, ha1), tail)
	  in failwith (Printf.sprintf "Expected failure on %s but got %s" (show_tail tail) (show_tail result))
	end
      with Errors.ProgramError _ -> ()

    let test_one normaliser tail expected =
      let ha0 = "X" in
      let ha1 = "X" in
      let (hd, result) = normaliser (q(ha0, ha1), tail)
      in begin
	(* head preserved? *)
	check_eq (function x -> x) (Literal.show hd) (Literal.show (q(ha0, ha1)));
	check_eq (function x -> x) (show_tail result) (show_tail expected);
      end

    let test' normaliser tail expected () =
      (* permute the tail *)
      let rec try_permutations current to_go =
	match to_go with
	  []	-> test_one normaliser current expected
	| _	-> begin
	  let rec do_try choices rest =
	    match choices with
	      []	-> ()
	    | h::tl	-> begin
	      try_permutations (h::current) (rest @ tl);
	      do_try tl (h::rest)
	    end
	  in do_try to_go []
	end
      in try_permutations [] tail

    let test = test' AP.select
    let test_n = test' Rule.normalise

    let test_trivial = test [] []
    let test_singleton = test  [p("X")] [p("X")]
    let test_singleton_fail = test_fail [eq("X", "X")] (* which is why we need to insert atom() first, see test_n_singleton *)

    let test_reorder_two_for_check =
      test
	[q("X", "Y"); p("Y")]
	[q("X", "Y"); p("Y")]

    let test_reorder_atom_for_check =
      test
	[atom("X"); p("X")]
	[p("X"); atom("X")]

    let test_reorder_delta =
      test
	[delta_r("X", "Y"); q("X", "Y")]
	[delta_r("X", "Y"); q("X", "Y")]

    let test_eq_0 =
      test
	[eq("X", "Y"); atom("X")]
	[atom("X"); leq "bf" ("X", "Y")]

    let test_eq_1 =
      test
	[eq("X", "Y"); p("Y"); atom("X")]
	[p("Y"); leq "fb" ("X", "Y"); atom("X")]

    let test_assign =
      test
	[p("X"); assign("X", "foo");]
	[assign("X", "foo"); p("X");]

    (* with normalisation *)

    let test_n_singleton = test_n [eq("X", "X")] [atom("X"); leq "bb" ("X", "X")]

    let test_n_eq_0 =
      test_n
	[eq("X", "Y"); atom("X")]
	[atom("X"); leq "bf" ("X", "Y")]

    let test_n_eq_1 =
      test_n
	[eq("X", "Y"); p("Y"); atom("X")]
	[p("Y"); leq "fb" ("X", "Y")]

    let test_n_add =
      test_n
	[atom("Y"); add("X", "Y", "Z"); atom("X"); q("X", "Y"); atom("Z")]
	[q("X", "Y"); ladd "bbf" ("X", "Y", "Z")]

    let test_n_concat =
      test_n
	[assign("Z", "foobar"); concat("X", "Y", "Z");]
	[assign("Z", "foobar"); lconcat "ffb" ("X", "Y", "Z");]

    let test_n_neg =
      test_n
	[Literal.neg (p("X")); q("X", "Y");]
	[q("X", "Y"); Literal.neg (p("X"));]
  end

let all_tests = "access-modes" >:::
  [
    "access-modes-simple-0" >:: AccessMode.test_simple_0;
    "access-modes-simple-1" >:: AccessMode.test_simple_1;
    "access-modes-simple-2" >:: AccessMode.test_simple_2;
    "access-modes-simple-3" >:: AccessMode.test_simple_3;
    "access-modes-simple-4" >:: AccessMode.test_simple_4;
    "access-modes-long-0" >:: AccessMode.test_long_0;
    "access-modes-long-1" >:: AccessMode.test_long_1;
    "access-modes-long-2" >:: AccessMode.test_long_2;
    "access-modes-multi-0" >:: AccessMode.test_multi_0;
    "access-modes-wildcards-0" >:: AccessMode.test_wildcards_0;
    "access-modes-wildcards-1" >:: AccessMode.test_wildcards_1;
    "access-modes-fail-0" >:: AccessMode.test_fail_0;

    "adapter-cont" >:: Adapter.test_cont;
    "adapter-set" >:: Adapter.test_set;
    "adapter-get" >:: Adapter.test_get;

    "link-P" >:: Link.test_p0;
    "link-eq-0" >:: Link.test_eq_0;
    "link-eq-1" >:: Link.test_eq_1;
    "link-eq-2" >:: Link.test_eq_2;
    "link-eq-fail" >:: Link.test_eq_fail;

    "expensive-estimate-P" >:: Expensive.test_p_check;
    "expensive-estimate-P" >:: Expensive.test_p_contains_0;
    "expensive-estimate-P" >:: Expensive.test_p_contains_1;
    "expensive-estimate-P" >:: Expensive.test_p_contains_2;
    "expensive-estimate-delta" >:: Expensive.test_delta_contains;
    "expensive-estimate-primop-eq-0" >:: Expensive.test_primop_eq_0;
    "expensive-estimate-primop-eq-1" >:: Expensive.test_primop_eq_1;
    "expensive-estimate-primop-eq-2" >:: Expensive.test_primop_eq_2;

    "ap-trivial" >:: APath.test_trivial;
    "ap-singleton" >:: APath.test_singleton;
    "ap-singleton-fail" >:: APath.test_singleton_fail;
    "ap-reorder-two-for-check-0" >:: APath.test_reorder_two_for_check;
    "ap-reorder-atom-for-check-0" >:: APath.test_reorder_atom_for_check;
    "ap-reorder-delta-to-head-0" >:: APath.test_reorder_delta;
    "ap-reorder-to-eq-check-0" >:: APath.test_eq_0;
    "ap-reorder-to-eq-check-1" >:: APath.test_eq_1;
    "ap-reorder-after-assign" >:: APath.test_assign;

    "norm-eq-check-0" >:: APath.test_n_eq_0;
    "norm-eq-check-1" >:: APath.test_n_eq_1;
    "norm-singleton" >:: APath.test_n_singleton;
    "norm-add" >:: APath.test_n_add;
    "norm-concat" >:: APath.test_n_concat;
    "norm-neg" >:: APath.test_n_neg;
  ]

let _ = run_test_tt_main (all_tests)
