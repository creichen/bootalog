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

include Base_literal
module PrimopInterface = Primop_interface

let access_modes (vars_before : VarSet.t) ((_, body) : t) =
  let access_mode (var) =
    if VarSet.contains vars_before var
    then PrimopInterface.Bound
    else PrimopInterface.Free
  in List.map access_mode (Array.to_list body)


(*
let variables_bound_after (vars_before : VarSet.t) ((pred, body) as literal : literal) =
  match pred with
    (Predicate _ | DeltaPredicate _)	-> Some (VarSet.union (vars_before, VarSet.of_array body)) (* trivially binds all *)
  | Primop (_, primop_id)			->
    let primop = Primops.get primop_id
    in match PrimopInterface.get_access_mode (access_modes vars_before literal) with
      None	-> None
    | 

*)
