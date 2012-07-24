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

 The author can be reached as "creichen" at the usual gmail server.

***************************************************************************)

exception Positional_after_nominal of string
exception Duplicate_label of string

type t = {
  positional: int;
  nominal: string list;
}

let make (positional : int) (nominal : string list) = {
  positional = positional; nominal = nominal;
}

(* label array must not be pre-sorted *)
let check_positional_after_nominal (label_array) =
  let rec check_nom (last) (la) =
    match la with
      []		-> ()
    | (Some n)::tl	-> check_nom n tl
    | None::_		-> raise (Positional_after_nominal (last))
  in
  let rec check_pos (la) =
    match la with
      []	-> ()
    | None::tl	-> check_pos tl
    | _		-> check_nom "*dummy*" (la)
  in check_pos (Array.to_list label_array)

(* label array must be pre-sorted already *)
let from_labels (label_array) =
  let check_sorted (last) (next) =
    if last = ""
    then ()
    else let comparison = String.compare last next
	 in if comparison = 0 then raise (Duplicate_label last)
	   else if comparison > 0 then failwith "label array not presorted in from_labels"
  in

  let get_nominal count rest =
    let rec get last la =
      match la with
	[]		-> []
      | (Some a)::tl	-> begin check_sorted last a; a::(get a tl) end
      | None::_		-> failwith "label array not presorted in from_labels"
    in (count, get "" rest)
  in
  let rec get_positional count la =
    match la with
      []	-> (count, [])
    | None::tl	-> get_positional (count + 1) (tl)
    | _		-> get_nominal (count) (la)
  in let (num, labels) = get_positional 0 (Array.to_list (label_array))
     in { positional = num; nominal = labels }

let show (signature : t) =
  String.concat "," ((string_of_int (signature.positional))::(signature.nominal))
