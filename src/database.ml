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
      table		: (predicate_symbol, table) Hashtbl.t }

let create table_generator =
  { generate	= table_generator;
    table		= Hashtbl.create (47) }

let new_table t predicate_symbol =
  let tbl = t.generate ()
  in begin
    Hashtbl.replace t.table predicate_symbol tbl;
    tbl
  end

let replace_table t predicate_symbol table =
  Hashtbl.replace t.table predicate_symbol table

let has_table t predicate_symbol =
  try begin ignore (Hashtbl.find t.table predicate_symbol);
    true
  end
  with Not_found -> false

let get_table t predicate_symbol =
  if has_table t predicate_symbol
  then new_table t predicate_symbol
  else Hashtbl.find t.table predicate_symbol
