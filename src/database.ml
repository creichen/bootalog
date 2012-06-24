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
type table = Combined_table.t


type t =
    { generate	: unit -> table;
      table	: (predicate, table) Hashtbl.t }

let create table_generator =
  { generate	= table_generator;
    table	= Hashtbl.create (47) }

let predicates table =
  let add a _ b = PredicateSet.add a b
  in Hashtbl.fold add table.table PredicateSet.empty

let new_table t predicate =
  let tbl = t.generate ()
  in begin
    Hashtbl.replace t.table predicate tbl;
    tbl
  end

let replace_table t predicate table =
  Hashtbl.replace t.table predicate table

let remove_table t predicate =
  Hashtbl.remove t.table predicate

let has_table t predicate =
  try begin ignore (Hashtbl.find t.table predicate);
    true
  end
  with Not_found -> false

let get_table t predicate =
  if has_table t predicate
  then Hashtbl.find t.table predicate
  else new_table t predicate

let show db =
  let predicates = predicates db in
  let show_table = Combined_table.show_tabular in
  let predicates_list = List.sort Predicate.compare (PredicateSet.to_list predicates) in
  let show_table (psym) =
    (Predicate.show psym) ^ ":\n" ^
      show_table (get_table db psym)
  in String.concat "\n" (List.map show_table predicates_list)

let equal db1 db2 =
  let ps1 = predicates db1 in
  let ps2 = predicates db2 in
  if not (PredicateSet.equal ps1 ps2)
  then false
  else let check_table psym =
	 (* Slow hack *)
	 Combined_table.show_tabular (get_table db1 psym)
	 = Combined_table.show_tabular (get_table db2 psym)
       in PredicateSet.for_all check_table ps1

(* let add_fact db (p, atom) = *)
(*   ... (\* fixme and write test *\) *)
