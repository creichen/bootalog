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

type env = Env.t

module Table = Combined_table
type table = Table.t

let eval_rule (pred_lookup : predicate_symbol -> table) ((head_p, head_vars), tail) =
  let bind_final (env) =
    let atoms = List.map (Env.find env) head_vars
    in Table.insert (pred_lookup head_p) atoms
  in
  let rec bind_next (list : predicate list) (env : env) =
    match list with
	[]		-> bind_final env
      | (p,body)::tl	-> Table.bind_all (pred_lookup p) body env (bind_next tl)
  in bind_next tail (Env.fresh ())

let eval_stratum db ({ pss; base; delta } : semi_naive_stratum) =
  let pss_nr = PredicateSymbolSet.cardinal pss in
  let old_tables = Hashtbl.create (pss_nr) in
  let delta_tables = Hashtbl.create (pss_nr) in
  let delta_lookup_tables = Hashtbl.create (pss_nr) in
  let join_tables = Hashtbl.create (pss_nr) in
  let was_updated_var = ref false in
  let init_replace_table predicate_symbol =
    let old_table = Database.get_table db predicate_symbol in
    let delta_table = Simple_table.create () in
    let join_table = Table.create_delta' (was_updated_var,
					  delta_table,
					  old_table)
    in let t : table = join_table 
       in begin
	 Hashtbl.replace old_tables predicate_symbol old_table;
	 Hashtbl.replace delta_tables predicate_symbol delta_table;
	 Hashtbl.replace join_tables predicate_symbol join_table;
	 Database.replace_table db predicate_symbol t
       end in
  let advance_delta_table predicate_symbol =
    let new_delta_table = Simple_table.create () in
    begin
      Hashtbl.replace delta_lookup_tables predicate_symbol (Hashtbl.find delta_tables predicate_symbol);
      Table.update_delta (Hashtbl.find join_tables predicate_symbol) new_delta_table;
      Hashtbl.replace delta_tables predicate_symbol new_delta_table
    end
  in
  let recover_original_table predicate_symbol =
    Database.replace_table db predicate_symbol (Hashtbl.find old_tables predicate_symbol)
  in
  let lookup_predicate_symbol ps =
    match ps with
	DeltaSym s	-> Table.SimpleT (Hashtbl.find delta_lookup_tables ps)
      | _		-> Database.get_table db ps
  in
  let eval_rules rules =
    List.iter (eval_rule lookup_predicate_symbol) rules
  in begin
    PredicateSymbolSet.iter init_replace_table pss;
    eval_rules (base);
    while !was_updated_var do
      was_updated_var := false;
      PredicateSymbolSet.iter advance_delta_table pss;
      eval_rules (delta)
    done;
    PredicateSymbolSet.iter recover_original_table pss
  end
    
let eval db program =
  List.iter (eval_stratum db) program
