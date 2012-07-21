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
open Error_test_helper

let pset (list : predicate list) : PredicateSet.t = PredicateSet.from_list (list)

let predicate_set = pset [psP; psQ; psR] (* psS only used for stratum generation *)


let make_deptable (pairlist : (Predicate.t * Predicate.t list) list) : DepTable.t * DepTable.t =
  let deptable = DepTable.create (7) in
  let rev_deptable = DepTable.create (7) in
  let add_many (k, vs) =
    let add_one v = begin
      DepTable.insert deptable k v;
      DepTable.insert rev_deptable v k;
    end
    in List.iter add_one vs
  in begin
    List.iter add_many pairlist;
    (deptable, rev_deptable)
  end

let all_ruleset_orders ruleset cont =
  let rec pick current rest =
    let rec pick_next r others =
      match r with
	  []	-> ()
	| h::tl	-> begin
	  pick (h::current) (others @ tl);
	  pick_next tl (h::others)
	end
    in match rest with
	[]	-> cont current
      | _	-> pick_next rest []
  in pick [] ruleset

let check_deps def_expectations default_ruleset () =
  let check_with_ruleset ruleset =
    let ruleset_str = RuleSet.show ruleset in
    let (expected_defs, expected_uses) = make_deptable def_expectations in
    let (defs, uses) = compute_rule_dependencies ruleset
    in let diff_msg = "mismatch:\n"
	 ^ "  def-expect: " ^ (DepTable.show expected_defs) ^ "\n"
	 ^ "  def-actual: " ^ (DepTable.show defs) ^ "\n"
	 ^ "  use-expect: " ^ (DepTable.show expected_uses) ^ "\n"
	 ^ "  use-actual: " ^ (DepTable.show uses) ^ "\n"
	 ^ "with ruleset =\n" ^ ruleset_str ^ "\n"
       in begin
	 check_eq ("defs: " ^ diff_msg) true (DepTable.equal expected_defs defs);
	 check_eq ("uses: " ^ diff_msg) true (DepTable.equal expected_uses uses);
       end
  in all_ruleset_orders default_ruleset check_with_ruleset

let check_eqclusters deps (expected : ((* preds *) predicate list * (* defs *) predicate list * (* uses *) predicate list) list) () =
  let mk_expected (e_preds, e_defs, e_uses) =
    { preds = pset e_preds; defs = pset e_defs; uses = pset e_uses } in
  let expected = List.map mk_expected expected in
  let actual = equivalence_clusters (make_deptable (deps)) (predicate_set) in
  let show_l strata =
    let show_one stratum = "    " ^ (DefUseStratum.show stratum) ^ "\n"
    in String.concat "" (List.map show_one strata)
  in
  let diff_msg = "mismatch:\n"
    ^ "  expected:\n" ^ (show_l expected)
    ^ "  actual:\n" ^ (show_l actual)
  in check_eq diff_msg true (DefUseStratum.equal_strata_with_set_semantics expected actual)


let check_stratum pss_list ruleset base_expect delta_expect () =
  let pss = PredicateSet.from_list pss_list in
  let expected_stratum = make_stratum_from_list pss_list base_expect delta_expect in
  let actual_stratum = make_stratum ruleset pss in
  let message = "mismatch:\n"
    ^ "---------- Expected:\n"
    ^ (Stratum.show (Stratum.normalise expected_stratum))
    ^ "---------- Actual:\n"
    ^ (Stratum.show (Stratum.normalise actual_stratum))
  in check_eq message true (Stratum.equal actual_stratum expected_stratum)

let check_stratify ruleset (expected_strata : (predicate list * rule list * rule list) list) () =
  let make_one_stratum (pss_list, base_expect, delta_expect) =
    make_stratum_from_list pss_list base_expect delta_expect in
  let expected_strata = List.map make_one_stratum expected_strata in
  let actual_strata = stratify ruleset in
  let message = "mismatch:\n"
    ^ "---------- Expected:\n"
    ^ (StratifiedRuleset.show (StratifiedRuleset.normalise expected_strata))
    ^ "---------- Actual:\n"
    ^ (StratifiedRuleset.show (StratifiedRuleset.normalise actual_strata))
  in check_eq message true (StratifiedRuleset.equal actual_strata expected_strata)

let ruleset0 = [
  pQ[x] <:- [pP[x]];
  pP[x] <:- [pR[x]];
]

let ruleset1 = [
  pQ[x] <:- [pS[x]];
  pQ[x] <:- [pP[x; y]; pQ[y]];
  pR[x] <:- [pQ[x]];
]

let ruleset2 = [
  pQ[x]		<:- [pS[x]];
  pR[x; y]	<:- [pQ[x]; pP[y; x]];
  pQ[x]		<:- [pR[x; y]; pQ[y]; pS[z]; pR[y; z]];
  pP[x; x]	<:- [pS[x]];
]

let ruleset3 = [
  pQ[x]		<:- [pS[x]];
  pR[x; y]	<:- [pQ[x]; pP[y; x]];
  pQ[x]		<:- [pR[x; y]; pQ[y]; pS[z]; pR[y; z]];
  pP[x; x]	<:- [Literal.neg (pS[x])];
]

let ruleset4error = [
  pP[x; x]	<:- [Literal.neg (pP[x; x])];
  pR[x; y]	<:- [pQ[x]; pP[y; x]];
  pQ[x]		<:- [pR[x; y]; pQ[y]; pS[z]; pR[y; z]];
]

let ruleset5error = [
  pR[x] <:- [pP[x]];
  pP[x]	<:- [pQ[x]];
  pQ[x] <:- [Literal.neg (pR[x])];
]

let all_tests = "stratification" >:::
  [
    "deptable-0" >:: (function () -> check_eq "d0" 0 (DepTable.size (DepTable.create 7)));
    "deptable-1" >:: (function () -> check_eq "d1" 1 (let d = DepTable.create 7 in begin DepTable.insert d psP psQ ; DepTable.size d end));
    "deptable-1-str" >:: (function () -> check_eq "d1" "{| p: {q} |}" (let d = DepTable.create 7 in begin DepTable.insert d psP psQ ; DepTable.show d end));
    "deps-0" >:: check_deps [(psP, [psQ])] [
      pQ[x] <:- [pP[x]]
    ];
    "deps-1" >:: check_deps [(psP, [psQ]); (psQ, [psQ])] [
      pQ[x] <:- [pP[x]; pQ[x; y]; pP[y]]
    ];
    "deps-transitive" >:: check_deps [(psP, [psQ]); (psR, [psP; psQ])] [
      pQ[x] <:- [pP[x]];
      pP[x] <:- [pR[x]];
    ];
    "deps-transitive-reflexive" >:: check_deps [(psP, [psQ]); (psR, [psP; psQ; psR])] [
      pQ[x] <:- [pP[x]];
      pP[x] <:- [pR[x]];
      pR[x] <:- [pR[x]];
    ];
    "deps-full" >:: check_deps [(psP, [psQ; psR; psP]); (psR, [psP; psQ; psR]); (psQ, [psP; psR; psQ])] [
      pQ[x] <:- [pP[x]];
      pP[x] <:- [pR[x]];
      pR[x] <:- [pQ[x]];
    ];
    "eqclusters-0" >:: check_eqclusters [(psP, [psQ])] [
      ([psQ], [],	[psP]);
      ([psP], [psQ],	[]);
      ([psR], [],	[]);
    ];
    "eqclusters-1" >:: check_eqclusters [(psP, [psQ; psP])] [
      ([psQ],	[],		[psP]);
      ([psP],	[psQ; psP],	[psP]);
      ([psR],	[],		[]);
    ];
    "eqclusters-transitive" >:: check_eqclusters [(psP, [psQ]); (psR, [psP; psQ])] [
      ([psR],	[psP; psQ],	[]);
      ([psQ],	[],		[psR; psP]);
      ([psP],	[psQ],		[psR]);
    ];
    "eqclusters-local-cluster" >:: check_eqclusters [(psP, [psQ; psP; psR]); (psQ, [psP; psQ; psR])] [
      ([psR],		[],			[psP; psQ]);
      ([psP; psQ],	[psP; psQ; psR],	[psP; psQ]);
    ];
    "eqclusters-full" >:: check_eqclusters [(psP, [psQ; psR; psP]); (psR, [psP; psQ; psR]); (psQ, [psP; psR; psQ])] [
      ([psP; psQ; psR],		[psP; psQ; psR],	[psP; psQ; psR])
    ];
    "stratum-0-ps" >:: check_stratum [psP; psS;] ruleset0 [
      pP[x] <:- [pR[x]];
    ] [
    ];
    "stratum-0-pr" >:: check_stratum [psP; psR;] ruleset0 [
      pP[x] <:- [pR[x]];
    ] [
      pP[x] <:- [dR[x]];
    ];
    "stratum-1-pq" >::
    check_stratum [psP; psQ;] ruleset1 [
      pQ[x] <:- [pS[x]];
      pQ[x] <:- [pP[x; y]; pQ[y]];
    ] [
      pQ[x] <:- [dP[x; y]; pQ[y]];
      pQ[x] <:- [dQ[y]; pP[x; y]];
    ];
    "stratum-2-qr" >:: check_stratum [psQ; psR;] ruleset2 [
      pQ[x]	<:- [pS[x]];
      pQ[x]	<:- [pR[x; y]; pQ[y]; pS[z]; pR[y; z]];
      pR[x; y]	<:- [pQ[x]; pP[y; x]];
    ] [
      pQ[x]	<:- [dR[x; y]; pQ[y]; pS[z]; pR[y; z]];
      pQ[x]	<:- [dQ[y]; pR[x; y]; pS[z]; pR[y; z]];
      pQ[x]	<:- [dR[y; z]; pR[x; y]; pQ[y]; pS[z]];
      pR[x; y]	<:- [dQ[x]; pP[y; x]];
    ];
    "stratify-0" >:: check_stratify ruleset0 [
      ([psP], [
	pP[x] <:- [pR[x]];
       ], [
      ]);
      ([psQ], [
	pQ[x] <:- [pP[x]];
       ], [
      ]);
    ];
    "stratify-1" >:: check_stratify ruleset1 [
      ([psQ], [
	pQ[x] <:- [pS[x]];
	pQ[x] <:- [pP[x; y]; pQ[y]];
       ], [
	pQ[x] <:- [dQ[y]; pP[x; y]];
      ]);
      ([psR], [
	pR[x] <:- [pQ[x]];
       ], [
      ]);
    ];
    "stratify-2" >:: check_stratify ruleset2 [
      ([psP], [
	pP[x; x]	<:- [pS[x]];
       ], [
      ]);
      ([psR; psQ], [
	pQ[x]		<:- [pS[x]];
	pR[x; y]	<:- [pQ[x]; pP[y; x]];
	pQ[x]		<:- [pR[x; y]; pQ[y]; pS[z]; pR[y; z]];
       ], [
	pQ[x]		<:- [dR[x; y]; pQ[y]; pS[z]; pR[y; z]];
	pQ[x]		<:- [dQ[y]; pR[x; y]; pS[z]; pR[y; z]];
	pQ[x]		<:- [dR[y; z]; pR[x; y]; pQ[y]; pS[z]];
	pR[x; y]	<:- [dQ[x]; pP[y; x]];
      ]);
    ];
    "stratify-3" >:: check_stratify ruleset3 [
      ([psP], [
	pP[x; x]	<:- [Literal.neg (pS[x])];
       ], [
      ]);
      ([psR; psQ], [
	pQ[x]		<:- [pS[x]];
	pR[x; y]	<:- [pQ[x]; pP[y; x]];
	pQ[x]		<:- [pR[x; y]; pQ[y]; pS[z]; pR[y; z]];
       ], [
	pQ[x]		<:- [dR[x; y]; pQ[y]; pS[z]; pR[y; z]];
	pQ[x]		<:- [dQ[y]; pR[x; y]; pS[z]; pR[y; z]];
	pQ[x]		<:- [dR[y; z]; pR[x; y]; pQ[y]; pS[z]];
	pR[x; y]	<:- [dQ[x]; pP[y; x]];
      ]);
    ];
    "stratify-4-fail" >:: expect_errors
                             [Errors.StratificationFailed (psP, [(psP, pP[x; x] <:- [Literal.neg (pP[x; x])])])]
                             (check_stratify ruleset4error []);
    "stratify-5-fail" >:: expect_errors
                             [Errors.StratificationFailed (psR,
    							   [
    							     (psP, pR[x] <:- [pP[x]]);
    							     (psQ, pP[x] <:- [pQ[x]]);
    							     (psR, pQ[x] <:- [Literal.neg (pR[x])]);
    							   ])]
                             (check_stratify ruleset5error []);
  ]

let _ = run_test_tt_main (all_tests)
