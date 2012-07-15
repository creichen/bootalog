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

include Base_rule

module AccessPath = Access_path

let normalise (rule) =
  (* We perform access path selection under the assumption that `atom' access to a bound variable is very cheap. *)
  let (head, tail_selected) = AccessPath.select (add_atoms (rule)) in
  (* We realise this by filtering out all accesses to bound atoms. *)
  let rec filter_accesses bound_vars t =
    match t with
      []				-> []
    | ((p, args) as literal)::tl	->
      if p = Predicate.atom && VarSet.contains bound_vars (Array.get args 0)
      then filter_accesses bound_vars tl	(* skip: variable is already bound *)
      else literal::(filter_accesses (VarSet.union (bound_vars) (BaseLiteral.vars literal)) tl)
  in (head, filter_accesses VarSet.empty tail_selected)
