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
let create () = Hashtbl.create (17)

let contains table tuple = Hashtbl.mem table tuple

let insert table tuple = Hashtbl.replace table tuple ()
let insert' table tuple = insert table (Array.of_list tuple)
let remove table tuple = Hashtbl.remove table tuple

let show table =
  let extract tuple _ body = (Tuple.show tuple) :: body
  in
  let body_list : string list = Hashtbl.fold (extract) table []
  in "{" ^  String.concat ", " (body_list) ^ "}"

let show_tabular table =
  let extract_sizes tuple _ best_sizes = Tuple.merge_string_sizes (Tuple.string_sizes tuple) best_sizes in
  let sizes = Hashtbl.fold extract_sizes table [] in
  let sizes_up = List.map (function a -> a + 2) sizes in
  let extract tuple _ body = ("  " ^ (Tuple.show_padded sizes_up tuple) ^ "\n") :: body
  in
  let body_list : string list = Hashtbl.fold (extract) table [] in
  let body_list = List.sort String.compare body_list
  in String.concat "" (body_list)

type bind_action =
    Bind
  | Check

let bind_all table variables env (continuation) =
  let size = Array.length variables in
  let actions = Array.create size Check in
  let bound_vars = ref VarSet.empty in
  let check_only = ref true in
  let get_from_env i =
    let var = Array.(*unsafe_*)get variables i in
    try Env.find env var
    with Not_found ->
      begin
	if not (VarSet.mem var (!bound_vars))
	then begin
	  Array.(*unsafe_*)set actions i Bind;
	  bound_vars := VarSet.add var (!bound_vars);
	  check_only := false;
	end;
	Atom.dummy
      end
  in
  let values = Array.init size get_from_env in
  let finish () = continuation env in
  if !check_only
  then
    if contains table values
    then finish ()
    else ()
  else (* must iterate *)
      begin
	let try_tuple tuple () =
	  let rec bind_at i =
	    if i >= size
	    then finish ()
	    else
	      let cont () = bind_at (i+1) in
	      let v : atom = Array.(*unsafe_*)get tuple i in
	      let var : variable = Array.(*unsafe_*)get variables i in
	      match Array.(*unsafe_*)get actions i with
		  Bind	-> begin Env.(*unsafe_*)bind env var v; cont() end
		| Check	-> if v = Env.find env var then cont ()
	  in bind_at 0
	in
	begin
	  Hashtbl.iter try_tuple table;
	  VarSet.iter (Env.unbind env) (!bound_vars);
	end
      end


