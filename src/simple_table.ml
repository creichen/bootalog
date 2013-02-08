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

open Base

type t = (tuple, unit) Hashtbl.t
let create (_) = Hashtbl.create (17)

let name = "simple"

let contains table tuple = Hashtbl.mem table tuple

let insert table tuple = Hashtbl.replace table tuple ()
let insert' table tuple = insert table (Array.of_list tuple)
let remove table tuple = Hashtbl.remove table tuple
let drop () = ()  (* rely on gc *)

let show table =
  let extract tuple _ body = (Tuple.show tuple) :: body
  in
  let body_list : string list = Hashtbl.fold (extract) table []
  in "{" ^  String.concat ", " (body_list) ^ "}"

let show_tabular table =
  let a_tuple = ref None in
  let () =
    let get_a_tuple tuple _ () =
      begin
	a_tuple := Some (tuple);
	raise Not_found
      end
    in try
	 Hashtbl.fold get_a_tuple table ()
      with Not_found -> ()
  in let a_tuple = Option.value_of (!a_tuple) in
  let extract_sizes tuple _ best_sizes = Tuple.Show.merge_string_sizes (Tuple.Show.string_sizes tuple) best_sizes in
  let sizes = Hashtbl.fold extract_sizes table [] in
  let sizes = Tuple.Show.merge_string_sizes (Tuple.Show.label_sizes (a_tuple)) (sizes) in
  let sizes_up = List.map (function a -> a + 2) sizes in
  let extract tuple _ body = ("  " ^ (Tuple.Show.show_padded_tuple sizes_up tuple) ^ "\n") :: body
  in
  let body_list : string list = Hashtbl.fold (extract) table [] in
  let body_list = List.sort String.compare body_list in
  let header = (Tuple.Show.show_padded_labels sizes_up a_tuple)
  in   ("  " ^ header ^ "\n")
     ^ ("  " ^ (String.make (String.length header) '-') ^ "\n")
     ^ String.concat "" (body_list)

let bind_all = Table_support.bind_all (Hashtbl.iter) (contains)

let drop _ = ()
