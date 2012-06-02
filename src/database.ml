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
      table	: (predicate_symbol, table) Hashtbl.t }

let create table_generator =
  { generate	= table_generator;
    table	= Hashtbl.create (47) }

let predicate_symbols table =
  let add a _ b = PredicateSymbolSet.add a b
  in Hashtbl.fold add table.table PredicateSymbolSet.empty

let new_table t predicate_symbol =
  let tbl = t.generate ()
  in begin
    Hashtbl.replace t.table predicate_symbol tbl;
    tbl
  end

let replace_table t predicate_symbol table =
  Hashtbl.replace t.table predicate_symbol table

let remove_table t predicate_symbol =
  Hashtbl.remove t.table predicate_symbol

let has_table t predicate_symbol =
  try begin ignore (Hashtbl.find t.table predicate_symbol);
    true
  end
  with Not_found -> false

let get_table t predicate_symbol =
  if has_table t predicate_symbol
  then Hashtbl.find t.table predicate_symbol
  else new_table t predicate_symbol

let show db =
  let predicate_symbols = predicate_symbols db in
  let show_table = Combined_table.show_tabular in
  let predicate_symbols_list = List.sort PredicateSymbol.compare (PredicateSymbolSet.to_list predicate_symbols) in
  let show_table (psym) =
    (PredicateSymbol.show psym) ^ ":\n" ^
      show_table (get_table db psym)
  in String.concat "\n" (List.map show_table predicate_symbols_list)

let equal db1 db2 =
  let ps1 = predicate_symbols db1 in
  let ps2 = predicate_symbols db2 in
  if not (PredicateSymbolSet.equal ps1 ps2)
  then false
  else let check_table psym =
	 (* Slow hack *)
	 Combined_table.show_tabular (get_table db1 psym)
	 = Combined_table.show_tabular (get_table db2 psym)
       in PredicateSymbolSet.for_all check_table ps1
