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


module Stratification =
struct
  module DepTable =
  struct
    let create = Hashtbl.create
    let lookup tbl (key : predicate_symbol) = try Hashtbl.find tbl key
      with Not_found -> PredicateSymbolSet.empty
    let insert tbl (key : predicate_symbol) (element : predicate_symbol) = Hashtbl.replace tbl key (PredicateSymbolSet.add element (lookup tbl key))
    let insert_all tbl (key : predicate_symbol) elements = Hashtbl.replace tbl key (PredicateSymbolSet.union (lookup tbl key) elements)
    let iter = Hashtbl.iter
  end

  let compute_rule_dependencies (ruleset) =
    let table_new () = DepTable.create (List.length (ruleset))
    in
    let all_defs = table_new () in
    let all_uses = table_new () in
    let new_defs = table_new () in
    let new_uses = table_new ()
    in
    let add def_tbl use_tbl head_p body_ps =
      let add_one_use_dependency (definer : predicate_symbol) = DepTable.insert def_tbl definer head_p
      in begin
	DepTable.insert_all use_tbl head_p body_ps;
	PredicateSymbolSet.iter add_one_use_dependency body_ps
      end
    in
    let insert_base (((predicate, _), _) as rule) =
      let body_predicates = predicate_symbol_set_body (rule)
      in add new_defs new_uses predicate body_predicates
    in (* initialise new_defs and new_uses *)
    let () = List.iter insert_base ruleset
    in (* initial set of dependencies computed, now compute closure *)
    let rec add_defs_uses (defs, uses) =
      if Hashtbl.length (defs) = 0 && Hashtbl.length (uses) = 0
      then ()
      else let new_defs = table_new () in
	   let new_uses = table_new () in
	   let add_new ((all_tbl, all_co_tbl), (new_tbl, new_co_tbl)) predicate new_elements =
	     let really_new = PredicateSymbolSet.diff (DepTable.lookup all_tbl predicate) new_elements
	     in
	     add all_tbl all_co_tbl predicate really_new;
	     add new_tbl new_co_tbl predicate really_new
	   in
	   let add_all (((all_tbl, all_co_tbl), (new_tbl, new_co_tbl)) as base) =
	     DepTable.iter (add_new base) new_tbl
	   in begin add_all ((all_defs, all_uses), (new_defs, new_uses));
	     add_all ((all_uses, all_defs), (new_uses, new_defs));
	     add_defs_uses (new_defs, new_uses) (* tail recurse *)
	   end
    in begin
      add_defs_uses (new_defs, new_uses);
      (all_defs, all_uses)
    end

  type def_use_stratum = { preds : PredicateSymbolSet.t;
			   defs  : PredicateSymbolSet.t;
			   uses  : PredicateSymbolSet.t }

  let equivalence_clusters (defs, uses) (predicate_set) =
    let cluster_map = DepTable.create (PredicateSymbolSet.cardinal (predicate_set))
    in let clusters : def_use_stratum list ref = ref []
    in
    let encluster (var) =
      if PredicateSymbolSet.is_empty (DepTable.lookup cluster_map var)
      then let cluster = let proto_cluster = PredicateSymbolSet.inter (DepTable.lookup defs var) (DepTable.lookup uses var)
			 in
			 if PredicateSymbolSet.is_empty proto_cluster
			 then PredicateSymbolSet.singleton var
			 else proto_cluster
	   in let add_cluster_item (key) = DepTable.insert_all cluster_map key cluster
	      in begin
		PredicateSymbolSet.iter add_cluster_item cluster;
		clusters := { preds = cluster; defs = DepTable.lookup defs var; uses = DepTable.lookup uses var } :: !clusters
	      end
      else ()
    in begin
      PredicateSymbolSet.iter encluster predicate_set;
      (!clusters (* unsorted *), cluster_map)
    end

  let sort_equivalence_clusters (strata : (* unsorted *) def_use_stratum list) =
    let compare (stratum0) (stratum1) =
      if PredicateSymbolSet.equal stratum0.preds stratum1.preds
      then 0
      else if not (PredicateSymbolSet.is_empty (PredicateSymbolSet.inter stratum1.uses stratum0.defs))
      then -1 (* stratum1 depends on stratum0, so stratum0 should be left*)
      else 1
    in List.sort compare strata

  let predicate_symbol_set_from_ruleset (ruleset) =
    List.fold_left (function set -> function ((p, _), _) -> PredicateSymbolSet.add p set) PredicateSymbolSet.empty ruleset

  type semi_naive_stratum =
      { pss	: PredicateSymbolSet.t;
	base	: rule list;
	delta	: rule list }

  let make_stratum (ruleset) (pss) =
    let predicate_is_relevant (p, _)	= PredicateSymbolSet.mem p pss in
    let gen_stratum (snstratum) ((head, body) as rule : rule) =
      if not (predicate_is_relevant (head))
      then snstratum
      else let rec find_deltas (body_prefix, body_tail) =
	     match body_tail with
		 []	-> []
	       | h::tl	->
		 if predicate_is_relevant h
		 then (Predicate.delta (head), [Predicate.delta(h)] @ List.rev (body_prefix) @ body_tail) :: find_deltas (h::body_prefix, tl)
		 else find_deltas (h::body_prefix, tl)
	   in let delta_rules = find_deltas ([], body)
	      in {
		pss	= snstratum.pss;
		base	= rule :: snstratum.base;
		delta	= delta_rules @ snstratum.delta
	      }
    in List.fold_left gen_stratum { pss = pss; base = []; delta = [] }

  let stratify (ruleset: ruleset) =
    (* (\* #1: Ground the rules *\) *)
    (* let ruleset = map ground_rule ruleset in *)
    (* #2: dependencies *)
    let (defs, uses) = compute_rule_dependencies (ruleset) in
    (* #3: cluster *)
    let predicate_set = predicate_symbol_set_from_ruleset (ruleset) in
    let (unsorted_cluster_list, _) = equivalence_clusters (defs, uses) (predicate_set) in
    (* #4 stratify *)
    let strata = sort_equivalence_clusters (unsorted_cluster_list) in
    (* #5 add delta rules for semi-naive evaluation, constructing a stratified_ruleset *)
    let stratum_predicate_sets = List.map (function s -> s.preds) strata
    in
    List.map (make_stratum ruleset) stratum_predicate_sets

end

(* Evaluation *)

type env = Env.t

module type TableSig =
  sig
    type t
    val create : unit -> t
    val contains : t -> tuple -> bool
    val insert : t -> tuple -> unit
    val bind_all : t -> variable list -> env -> (env -> unit) -> unit
  end

module CombinedTable =
  struct
    type t =
	SimpleT of Simple_table.t
      | DeltaT of bool ref * Simple_table.t ref * Simple_table.t  (* tracks updates: flag if updated, middle element contains only updates *)

    let create () = SimpleT (Simple_table.create ())
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

    let bind_all table (variables : variable list) (env : env) (continuation : env -> unit) : unit =
      match table with
	  (SimpleT t | DeltaT (_, _, t)) -> Simple_table.bind_all t variables env continuation
  end

module Table = CombinedTable
type table = CombinedTable.t

module Database =
  struct
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
  end

module Eval =
struct
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

  type semi_naive_stratum =
      { pss	: PredicateSymbolSet.t;
	base	: rule list;
	delta	: rule list }

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
end
