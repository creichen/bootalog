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
include Base_literal
module PrimopInterface = Primop_interface

let variable_modes (vars_before : VarSet.t) ((_, body) : t) =
  let variable_mode (var) =
    if VarSet.contains vars_before var
    then PrimopInterface.Bound
    else PrimopInterface.Free
  in List.map variable_mode (Array.to_list body)

let get_access_mode (cmp) (bound_variable_set) (literal) (primop_id) =
  let variable_modes = variable_modes bound_variable_set literal in
  let access_modes = PrimopInterface.access_modes (variable_modes) (Primops.get primop_id) in
  let rec get_best (current_best) (list) =
    match list with
      []		-> current_best
    | amode::tl	->
      let next_best = match current_best with
	  None	-> Some amode
	| Some bmode	->
	  if cmp (amode.PrimopInterface.cost) (bmode.PrimopInterface.cost) < 0
	  then Some amode
	  else Some bmode
      in get_best next_best tl
  in get_best None access_modes

let get_best_access_mode = get_access_mode (PrimopInterface.compare_cost)
let get_worst_access_mode = get_access_mode (function a -> function b -> PrimopInterface.compare_cost b a)

(* Try to link primitive operations, picking the best (cheapest) match.
   May fail if there is no match (returns None).
*)
let link (vars_before : VarSet.t) ((predicate, body) as literal : t) : ((t * PrimopInterface.access_mode option) option) =
  let link_primop (primop_name, primop_id) =
    let variable_modes = variable_modes vars_before literal
    in match get_best_access_mode (vars_before) (literal) (primop_id) with
      None		-> None
    | Some amode	->
      let full_name = primop_name ^ "[" ^ (String.concat "" (List.map Primop_interface.show_variable_mode variable_modes)) ^ "]"
      in Some ((Predicate.Linked (full_name, primop_id, amode.PrimopInterface.evaluator), body),
	       Some amode)

  in match predicate with
    Predicate.Primop (a,b)	-> link_primop (a,b)
  | other			-> Some ((other, body), None)


let atom_predicate_entries = 1000
let base_predicate_entries = 100
let delta_predicate_entries = 10
let predicate_element_read_cost = 10
let predicate_element_check_cost = 25
let atom_bind_cost = 1

let estimate_access_cost (vars_before : VarSet.t) ((predicate, _) as literal : t) : PrimopInterface.access_cost =
  let compute_cost (expected_nr_of_entries, check_cost) = (* compute cost for regular EDB or IDB predicate *)
    let variable_modes = variable_modes vars_before literal in
    let is_check_only = List.for_all (function a -> a = PrimopInterface.Bound) variable_modes
    in if is_check_only
      then PrimopInterface.cost (check_cost)
      else PrimopInterface.cost (expected_nr_of_entries * predicate_element_read_cost)

  in match predicate with
    Predicate.Primop (_, id)	-> (value_of (get_worst_access_mode (vars_before) (literal) (id))).PrimopInterface.cost
  | Predicate.P _		-> (if predicate = Predicate.atom
                                    then compute_cost (atom_predicate_entries, 0)
                                    else compute_cost (base_predicate_entries, predicate_element_check_cost))
  | Predicate.Delta _		-> compute_cost (delta_predicate_entries, predicate_element_check_cost)
  | Predicate.Assign _		-> PrimopInterface.cost atom_bind_cost
  | Predicate.Linked _		-> failwith "estimate_access_cost shouldn't be called on linked predicates"

let link_and_get_access_cost (vars_before : VarSet.t) (literal : t) : ((t * PrimopInterface.access_cost) option) =
  match link (vars_before) (literal) with
    None		-> None
  | Some (a, Some c)	-> Some (a, c.PrimopInterface.cost)
  | Some (a, None)	-> Some (a, estimate_access_cost (vars_before) (literal))
