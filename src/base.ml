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

include Option

type atom = Atom.t
type tuple = Tuple.t

module BaseLiteral = Base_literal
module BaseRule = Base_rule

type predicate = Predicate.t
type base_predicate = Predicate.base_t
type fact = Fact.t
type variable = Variable.t
type literal = BaseLiteral.t
type rule = BaseRule.t

module VarSet = Var_set
module PredicateSet = Predicate_set
module RuleSet = Rule_set
module StratifiedRuleset = Stratified_ruleset

type ruleset = RuleSet.t
type stratum = Stratum.t


