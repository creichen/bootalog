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
open Stratification
open Stratum_test_helper
module DB = Database

let db elements =
  let db = DB.create (Combined_table.create) in
  let add_table (psym, tuples) =
    let table = DB.get_table db psym in
    let add_tuple tuple =
      Combined_table.insert table (Array.of_list (List.map (function _ -> Label.none) tuple), Array.of_list tuple)
    in List.iter add_tuple tuples
  in begin
    List.iter add_table elements;
    db
  end

let check_eq msg expected actual =
  assert_equal expected actual ?msg:(Some msg)

let test_eval_any start_db (evaluator_closure, show_closure) expected_db () =
  let actual_db = db start_db in
  let expected_db = db expected_db in
  begin
    evaluator_closure (actual_db);
    let message = "Mismatch in result database:\n"
      ^ "========== Actual:\n"
      ^ (DB.show actual_db)
      ^ "========== Expected:\n"
      ^ (DB.show expected_db)
      ^ "========== Rules:\n"
      ^ (show_closure ())
    in check_eq message true (DB.equal actual_db expected_db)
  end


let test_eval_rule start_db rule expected_db =
  let eval db = Eval.eval_rule (DB.get_table db) rule in
  let show () = Rule.show rule
  in test_eval_any start_db (eval, show) expected_db

let test_eval_stratum start_db (s0, s1, s2) expected_db =
  let stratum = (make_stratum_from_list s0 s1 s2) in
  let eval db = Eval.eval_stratum db stratum in
  let show () = Stratum.show (Stratum.normalise stratum)
  in test_eval_any start_db (eval, show) expected_db

let all_tests = "eval" >:::
  [
    "transfer-rule" >:: test_eval_rule [
      (psR, [["foo"]; ["bar"]])
    ] (pP[x] <:- [pR[x]]) [
      (psR, [["foo"]; ["bar"]]);
      (psP, [["foo"]; ["bar"]]);
    ];
    "filter-rule" >:: test_eval_rule [
      (psR, [["foo"]; ["bar"]]);
      (psQ, [["foo"]]);
    ] (pP[x] <:- [pR[x]; pQ[x];]) [
      (psR, [["foo"]; ["bar"]]);
      (psQ, [["foo"]]);
      (psP, [["foo"]]);
    ];
    "transitive-rule" >:: test_eval_rule [
      (psR, [["1"; "2"]; ["2"; "3"];])
    ] (pR[x; z] <:- [pR[x; y]; pR[y; z]]) [
      (psR, [["1"; "2"]; ["2"; "3"]; ["1"; "3"]])
    ];
    "pair-rule" >:: test_eval_rule [
      (psR, [["foo"]; ["bar"]])
    ] (pP[x; x] <:- [pR[x]]) [
      (psR, [["foo"]; ["bar"]]);
      (psP, [["foo"; "foo"]; ["bar"; "bar"]]);
    ];
    "primop-rule-eq" >:: test_eval_rule [
      (psR, [["foo"]; ["bar"]])
    ] (pP[x; y] <:- [pR[x]; pEq_bf[x; y]]) [
      (psR, [["foo"]; ["bar"]]);
      (psP, [["foo"; "foo"]; ["bar"; "bar"]]);
    ];
    "assign-rule" >:: test_eval_rule [
      (psR, [["foo"]; ["bar"]])
    ] (pP[x; y] <:- [pR[x]; pAssign "zero" [y]]) [
      (psR, [["foo"]; ["bar"]]);
      (psP, [["foo"; "zero"]; ["bar"; "zero"]]);
    ];
    "neg-rule" >:: test_eval_rule [
      (psP, [["foo"]; ["bar"]]);
      (psQ, [["foo"]; ["bar"]; ["quux"]]);
    ] (pR[x] <:- [pQ[x]; Literal.neg (pP[x])]) [
      (psP, [["foo"]; ["bar"]]);
      (psQ, [["foo"]; ["bar"]; ["quux"]]);
      (psR, [["quux"]]);
    ];
    "primop-rule-concat" >:: test_eval_rule [
    ] (pR[x] <:- [pAssign "foobar" [z]; pConcat_ffb[x; y; z]]) [
      (psR, [[""]; ["f"]; ["fo"]; ["foo"]; ["foob"]; ["fooba"]; ["foobar"]]);
    ];
    "stratum-transitive" >:: test_eval_stratum [
      (psR, [["1"; "2"]; ["2"; "3"]; ["3"; "4"]; ["4"; "5"];]);
    ] ([psQ], [
      pQ[x; z] <:- [pR[x; z]];
      pQ[x; z] <:- [pR[x; y]; pQ[y; z]];
    ], [
      pQ[x; z] <:- [dQ[x; y]; pR[y; z]];
    ]
    ) [
      (psR, [["1"; "2"]; ["2"; "3"]; ["3"; "4"]; ["4"; "5"]]);
      (psQ, [["1"; "2"]; ["2"; "3"]; ["3"; "4"]; ["4"; "5"];
	                 ["1"; "3"]; ["1"; "4"]; ["1"; "5"];
	                             ["2"; "4"]; ["2"; "5"];
			                         ["3"; "5"];
	    ]);
    ];
  ]

let _ = run_test_tt_main (all_tests)
