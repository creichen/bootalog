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

module Atom =
  struct
    type t = atom
    let show (s : string) = s
    let dummy = ""
  end

type tuple = atom array

module Tuple =
  struct
    type t = tuple
    let show elts = "(" ^ (String.concat ", " (List.map Atom.show (Array.to_list elts))) ^ ")"
    let sort = List.sort (Compare.array_collate (String.compare))
  end

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

type predicate = predicate_symbol * (variable array)

module Predicate =
  struct
    type t = predicate

    let delta (p, body) =
      (PredicateSymbol.delta p, body)

    let show (symbol, varlist) =
      PredicateSymbol.show(symbol) ^ "(" ^ (String.concat ", " (List.map Variable.show (Array.to_list varlist))) ^ ")"
    let compare = Compare.join (PredicateSymbol.compare) (Compare.array_collate Variable.compare)
  end

type rule = predicate * predicate list

module Rule =
  struct
    type t = rule
    let show (head, tail) = Predicate.show (head) ^ " :- " ^ (String.concat ", " (List.map Predicate.show tail)) ^ "."
    let compare = Compare.join (Predicate.compare) (Compare.collate (Predicate.compare))
    let equal a b = 0 = compare a b
  end

type ruleset = rule list

module VarSet = Set.Make(Variable)
let var_set_add' a b = VarSet.add b a
module RuleSet =
  struct
    type t = ruleset
    let show ruleset = String.concat "\n" (List.map Rule.show ruleset)
  end

module PredicateSymbolSet' = Set.Make(PredicateSymbol)
module PredicateSymbolSet =
  struct
    open PredicateSymbolSet'
    type t = PredicateSymbolSet'.t
    let empty = empty
    let add = add
    let add' a b = add b a
    let from_list = List.fold_left add' empty
    let iter = iter
    let fold = fold
    let union = union
    let inter = inter
    let cardinal = cardinal
    let is_empty = is_empty
    let singleton = singleton
    let contains = mem
    let equal = equal
    let for_all = for_all
    let diff = diff
    let show set =
      let show_one elt tail = (PredicateSymbol.show elt)::tail
      in let elts = fold show_one set [] in
	 "{" ^ (String.concat ", " elts) ^ "}"
  end

let varset_predicate (_, vars) = List.fold_left var_set_add' VarSet.empty vars
let varset_rule_head (head, _) = varset_predicate(head)
let varset_rule_body (_, body) = List.fold_left (fun map -> fun (_, vars) -> List.fold_left var_set_add' map vars) VarSet.empty body

let predicate_symbol_set_body (_, body) =  List.fold_left (fun map -> fun (p, _) -> PredicateSymbolSet.add p map) PredicateSymbolSet.empty body


type stratum =
    { pss	: PredicateSymbolSet.t;
      base	: rule list;
      delta	: rule list }

module Stratum =
  struct
    type t = stratum

    let show_n label stratum =
      let show_rules rules =
	String.concat "" (List.map (function rule -> "  " ^ Rule.show rule ^ "\n") rules)
      in ("== " ^ label ^ "<" ^ (PredicateSymbolSet.show stratum.pss) ^ ">}\n"
	  ^ "- base:\n"
	  ^ (show_rules stratum.base)
	  ^ "- delta:\n"
	  ^ (show_rules stratum.delta))

    let show = show_n ""

    let normalise stratum =
      { pss	= stratum.pss;
	base	= List.sort Rule.compare stratum.base;
	delta	= List.sort Rule.compare stratum.delta; }

    let equal stratum0 stratum1 =
      let s0 = normalise stratum0 in
      let s1 = normalise stratum1
      in (PredicateSymbolSet.equal s0.pss s1.pss
	  && s0.base = s1.base
	    && s0.delta = s1.delta)
  end

module StratifiedRuleset =
  struct
    type t = stratum list

    let show strata =
      let rec show_i i args =
	match args with
	    []		-> ""
	  | h::tl	-> (Stratum.show_n (Printf.sprintf "%i" i) h) ^ "\n" ^ (show_i (i+1) tl)
      in show_i 0 strata

    let normalise =
      List.map Stratum.normalise

    let equal =
      let rec is_eq r0 r1 =
	match (r0, r1) with
	    ([], [])	-> true
	  | (h0::tl0,
	     h1::tl1)	-> if Stratum.equal h0 h1 then is_eq tl0 tl1 else false
	  | _		-> false
      in is_eq
  end

