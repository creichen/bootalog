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

type t = Label.t array * Atom.t array (* of equal length *)
let show (labels, elts) =
  let showi (index : int) (atom : Atom.t) = Label.show_atom (Array.get labels index) (atom)
  in  "(" ^ (String.concat ", " (Array.to_list (Array.mapi showi elts))) ^ ")"

(* In practice, the labels should be invariant over all tuples we're trying to sort, but
 * we try to be general below. *)
let sort = List.sort (Compare.join (Compare.array_collate Label.compare) (Compare.array_collate String.compare))


module Show =
struct
  (* This module assumes that all tuples have the same labels. *)

  let string_sizes (_, tuple) =
    List.map (function t -> (String.length (Atom.show t))) (Array.to_list tuple)

  let label_sizes (tuple, _) =
    List.map (function t -> (String.length (Label.show t))) (Array.to_list tuple)

  let show_padded show sizes array =
    let rec s a b =
      match (a, b) with
	(size::sl, elt::al)	-> (Printf.sprintf "%-*s" size (show elt)) :: (s sl al)
      | (_, [])			-> []
      | ([], elt::al)		-> (show elt) :: (s [] al)
    in String.concat "" (s sizes (Array.to_list array))

  let show_padded_labels sizes (labels, _) =
    show_padded Label.show sizes labels

  let show_padded_tuple sizes (_, tuple) =
    show_padded Atom.show sizes tuple

  let merge_string_sizes sizes0 sizes1 =
    let rec m a b =
      match (a, b) with
	([], tl)	-> tl
      | (tl, [])	-> tl
      | (h1::tl1,
	 h2::tl2)	-> (if h1 > h2 then h1 else h2)::(m tl1 tl2)
    in m sizes0 sizes1
end

let positional (array) = (Array.map (function _ -> Label.none) array, array)
