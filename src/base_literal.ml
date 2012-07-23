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

module VarSet = Var_set

type body_t = (Label.t array * Variable.t array)
type t = Predicate.t * body_t

let delta (p, body) =
  (Predicate.delta p, body)

let show (symbol, (labels, vars)) =
  let showi (index : int) (var : Variable.t) = Label.show_var (Array.get labels index) (var)
  in  (Predicate.show (symbol)) ^ "(" ^ (String.concat ", " (Array.to_list (Array.mapi showi vars))) ^ ")"

let compare = Compare.join (Predicate.compare) (Compare.join (Compare.array_collate Label.compare) (Compare.array_collate Variable.compare))

let neg (p, body) = (Predicate.neg p, body)

let is_neg (p, _) = Predicate.is_neg (p)

let vars (_, (_, vars)) = VarSet.of_array (vars)
