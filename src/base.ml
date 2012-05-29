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

exception Malformed
exception NotADeltaTable
exception UnexpectedDeltaTable

type atom = string

type tuple = atom list

type predicate_symbol =
    PredicateSym of string
  | DeltaSym of string

module PredicateSymbol =
  struct
    type t = predicate_symbol

    let is_delta s =
      match s with
	  DeltaSym _	-> true
	| _		-> false

    let show (p) =
      match p with
	  PredicateSym p	-> p
	| DeltaSym d		-> "D[" ^ d ^ "]"

    let delta s =
      match s with
	  PredicateSym p	-> DeltaSym p
	| DeltaSym d		-> raise (Failure ("Attempted deltafication of delta`"^d^"'"))

    let compare l r =
      match (l, r) with
	  (PredicateSym _, DeltaSym _)		-> -1
	| (DeltaSym _, PredicateSym _)		-> 1
	| ((PredicateSym a,PredicateSym b)
	      | (DeltaSym a, DeltaSym b))	-> String.compare a b

  end

type variable = Variable.t

type predicate = predicate_symbol * (variable list)

module Predicate =
  struct
    type t = predicate

    let delta (p, body) =
      (PredicateSymbol.delta p, body)

    let show (symbol, varlist) =
      PredicateSymbol.show(symbol) ^ "(" ^ (String.concat ", " (List.map Variable.show varlist)) ^ ")"
    let compare = Compare.join (PredicateSymbol.compare) (Compare.collate Variable.compare)
  end

type rule = predicate * predicate list

module Rule =
  struct
    type t = rule
    let show (head, tail) = Predicate.show (head) ^ " :- " ^ (String.concat ", " (List.map Predicate.show tail)) ^ "."
    let compare = Compare.join (Predicate.compare) (Compare.collate (Predicate.compare))
  end

type ruleset = rule list

type stratum = { tables	: predicate_symbol list;
		 base	: ruleset;
		 delta	: ruleset }
type stratified_ruleset = stratum list

module VarSet = Set.Make(Variable)
let var_set_add' a b = VarSet.add b a
module RuleSet = Set.Make(Rule)
module PredicateSymbolSet = Set.Make(PredicateSymbol)
let predicate_symbol_set_add' a b = PredicateSymbolSet.add b a

(* let atoms_predicate_symbol = PredicateSym "Atoms" *)


let varset_predicate (_, vars) = List.fold_left var_set_add' VarSet.empty vars
let varset_rule_head (head, _) = varset_predicate(head)
let varset_rule_body (_, body) = List.fold_left (fun map -> fun (_, vars) -> List.fold_left var_set_add' map vars) VarSet.empty body

let predicate_symbol_set_body (_, body) =  List.fold_left (fun map -> fun (p, _) -> PredicateSymbolSet.add p map) PredicateSymbolSet.empty body
