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

module BaseLiteral = Base_literal
module VarSet = Var_set
module PredicateSet = Predicate_set

type literal = BaseLiteral.t

type t = literal * literal list

let show (head, tail) = BaseLiteral.show (head) ^ " :- " ^ (String.concat ", " (List.map BaseLiteral.show tail)) ^ "."
let compare = Compare.join (BaseLiteral.compare) (Compare.collate (BaseLiteral.compare))
let equal a b = 0 = compare a b

let body_predicates (_, body) =  List.fold_left (fun map -> fun (p, _) -> PredicateSet.add p map) PredicateSet.empty body
let body_neg_predicates (_, body) =  List.fold_left (fun map -> fun (p, _) -> if Predicate.is_neg (p) then PredicateSet.add ((*Predicate.non_negative*) p) map else map) PredicateSet.empty body
let body_nonnegativised_predicates (_, body) =  List.fold_left (fun map -> fun (p, _) -> PredicateSet.add (Predicate.non_negative p) map) PredicateSet.empty body
let head_vars (head, _) = BaseLiteral.vars (head)
let body_vars (_, body) = List.fold_left (fun map -> fun (_, vars) -> Array.fold_left VarSet.add' map vars) VarSet.empty body

let add_atoms ((head, tail) as rule : t) =
  let head_vars = head_vars (rule) in
  let body_vars = body_vars (rule) in
  let free_vars = VarSet.union head_vars body_vars in
  let add_atom_predicate var tail = (Predicate.atom, [| var |]) :: tail in
  let atoms = VarSet.fold add_atom_predicate free_vars [] in
  (head, atoms @ tail)


