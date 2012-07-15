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
open Stratum

type env = Env.t

module Table = Combined_table
type table = Table.t

let eval_rule (pred_lookup : predicate -> table) ((head_p, head_vars), tail) =
  let bind_final (env) =
    let atoms = Array.map (Env.find env) head_vars
    in Table.insert (pred_lookup head_p) atoms
  in
  let rec bind_next (list : literal list) (env : env) =
    match list with
      []						-> bind_final env
    | (((Predicate.P _) as p,body)::tl
	  | ((Predicate.Delta _) as p, body)::tl)	-> Table.bind_all (pred_lookup p) body env (bind_next tl)
    | (Predicate.Linked (_, _, evaluator), body)::tl	-> evaluator body env (bind_next tl)
    | (Predicate.Assign atom, body)::tl			-> let () = Env.bind env (Array.get body 0) atom
							   in bind_next tl env
    | (Predicate.Primop _, _)::_			-> failwith "Encountered unlinked Primop during rule evaluation"
  in bind_next tail (Env.fresh ())

let eval_stratum db ({ pss; base; delta } : stratum) =
  let pss_nr = PredicateSet.cardinal pss in
  let old_tables = Hashtbl.create (pss_nr) in
  let delta_tables = Hashtbl.create (pss_nr) in
  let delta_lookup_tables = Hashtbl.create (pss_nr) in
  let join_tables = Hashtbl.create (pss_nr) in
  let was_updated_var = ref false in
  let init_replace_table predicate =
    let old_table = Database.get_table db predicate in
    let delta_table = Simple_table.create () in
    let join_table = Table.create_delta' (was_updated_var,
					  delta_table,
					  old_table)
    in let t : table = join_table 
       in begin
	 Hashtbl.replace old_tables predicate old_table;
	 Hashtbl.replace delta_tables predicate delta_table;
	 Hashtbl.replace join_tables predicate join_table;
	 Database.replace_table db predicate t;
	 Database.replace_table db (Predicate.delta predicate) (Combined_table.from_simple delta_table);
       end in
  let advance_delta_table predicate =
    let new_delta_table = Simple_table.create () in
    let old_delta_table =
      try
	Hashtbl.find delta_lookup_tables predicate
      with Not_found ->  Simple_table.create ()
    in
    begin
      Hashtbl.replace delta_lookup_tables predicate (Hashtbl.find delta_tables predicate);
      Table.update_delta (Hashtbl.find join_tables predicate) new_delta_table;
      Hashtbl.replace delta_tables predicate new_delta_table;
      Database.replace_table db (Predicate.delta predicate) (Combined_table.from_simple old_delta_table);
      ignore (Table.SimpleT (Hashtbl.find delta_lookup_tables predicate));
    end
  in
  let recover_original_table predicate =
    Database.replace_table db predicate (Hashtbl.find old_tables predicate);
    Database.remove_table db (Predicate.delta predicate)
  in
  let lookup_predicate ps =
    match ps with
	Predicate.Delta s	-> Table.SimpleT (Hashtbl.find delta_lookup_tables (Predicate.P s))
      | _			-> Database.get_table db ps
  in
  let eval_rules rules =
    List.iter (eval_rule lookup_predicate) rules
  in begin
    PredicateSet.iter init_replace_table pss;
    eval_rules (base);
    while !was_updated_var do
      was_updated_var := false;
      PredicateSet.iter advance_delta_table pss;
      eval_rules (delta)
    done;
    PredicateSet.iter recover_original_table pss
  end
    
let eval db program =
  List.iter (eval_stratum db) program
