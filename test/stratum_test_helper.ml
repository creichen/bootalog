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
open Errors

module PI = Primop_interface

let psP = Predicate.P "p"
let psQ = Predicate.P "q"
let psR = Predicate.P "r"
let psS = Predicate.P "s"

let pred psym (args : variable list) = (psym, (Tuple.positional (Array.of_list args)))
let pP = pred psP
let pQ = pred psQ
let pR = pred psR
let pS = pred psS

let pEq_bf = 
  let pid : PI.id = value_of (PI.primop_id Primops.Sys.eq) in
  let amodes = Primops.get (pid) in
  let amode = List.hd (PI.access_modes [PI.Bound; PI.Free] amodes)
  in pred (Predicate.Linked ("=[bf]", pid, amode.PI.evaluator))
let pConcat_ffb = 
  let pid : PI.id = value_of (PI.primop_id Primops.Sys.concat) in
  let amodes = Primops.get (pid) in
  let amode = List.hd (PI.access_modes [PI.Free; PI.Free; PI.Bound] amodes)
  in pred (Predicate.Linked ("sys-concat[ffb]", pid, amode.PI.evaluator))
let pAssign v = pred (Predicate.Assign v)

let dpred psym (args : variable list) = (Predicate.delta (psym), (Tuple.positional (Array.of_list args)))
let dP = dpred psP
let dQ = dpred psQ
let dR = dpred psR
let dS = dpred psS

let x = "X"
let y = "Y"
let z = "Z"

let (<:-) head tail = (head, tail)

let check_eq msg expected actual =
  assert_equal expected actual ?msg:(Some msg)

let make_stratum_from_list pss_list base_expect delta_expect =
  let pss = PredicateSet.from_list pss_list in
  let expected_stratum = {
    Stratum.pss		= pss;
    Stratum.base	= base_expect;
    Stratum.delta	= delta_expect;
  } in expected_stratum

