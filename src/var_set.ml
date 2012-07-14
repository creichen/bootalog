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

module VarSet' = Set.Make(Variable)
include VarSet'

let contains set elt = mem elt set
let add' a b = add b a
let show set =
  let show_one elt tail = (Variable.show elt)::tail
  in let elts = fold show_one set [] in
     "{" ^ (String.concat ", " elts) ^ "}"

let of_array (array) = Array.fold_left add' empty array
let of_list (list) = List.fold_left add' empty list

