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
type env = Env.t

type t =
    SimpleT of Simple_table.t
  | DeltaT of bool ref * Simple_table.t ref * Simple_table.t  (* tracks updates: flag if updated, middle element contains only updates *)

let create () = SimpleT (Simple_table.create ())
let from_simple st = SimpleT st
let create_delta (flag, delta, rhs) = DeltaT (flag, ref delta, rhs)
let create_delta' (flag, delta, rhs) =
  match rhs with
      SimpleT t	-> create_delta (flag, delta, t)
    | _		-> raise UnexpectedDeltaTable

let contains (table) (tuple : tuple) : bool =
  match table with
      (SimpleT t | DeltaT (_, _, t)) -> Simple_table.contains t tuple

let update_delta (table) (new_delta_table : Simple_table.t) =
  match table with
      DeltaT (_, r, _)	-> r := new_delta_table
    | _			-> raise NotADeltaTable

let show (table) =
  let contents t = Simple_table.show t in
  match table with
      SimpleT t		-> "S:" ^ contents (t) ^ ""
    | DeltaT (u,d,t)	-> Printf.sprintf "D<%b>:%s / %s" (!u) (contents (!d)) (contents t)

let show_tabular (table) =
  let contents t = Simple_table.show_tabular t in
  match table with
      (SimpleT t | DeltaT (_, _, t))	-> contents t

let insert table (tuple: tuple) : unit =
  match table with
      SimpleT t				-> Simple_table.insert t tuple
    | DeltaT (update_flag, delta, t)	->
      if not (Simple_table.contains t tuple)
      then begin
	Simple_table.insert t tuple;
	Simple_table.insert (!delta) tuple;
	update_flag := true
      end

let remove table (tuple: tuple) : unit =
  match table with
      SimpleT t		-> Simple_table.remove t tuple
    | DeltaT _		-> raise (Failure "Attempted to remove entry from delta table!")

let insert' table (tuple) : unit =
  insert table (Array.of_list tuple)

let bind_all table (variables : variable array) (env : env) (continuation : env -> unit) : unit =
  match table with
      (SimpleT t | DeltaT (_, _, t)) -> Simple_table.bind_all t variables env continuation
