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
open Stratum

module DepTable =
struct
  type t = (Predicate.t, PredicateSet.t) Hashtbl.t

  let create = Hashtbl.create
  let lookup tbl (key : predicate) = try Hashtbl.find tbl key
    with Not_found -> PredicateSet.empty
  let insert tbl (key : predicate) (element : predicate) = Hashtbl.replace tbl key (PredicateSet.add element (lookup tbl key))
  let insert_all tbl (key : predicate) elements = Hashtbl.replace tbl key (PredicateSet.union (lookup tbl key) elements)
  let iter = Hashtbl.iter

  let keys table =
    let aggregate key _ set = PredicateSet.add key set
    in Hashtbl.fold aggregate table PredicateSet.empty

  let size = Hashtbl.length
  let is_empty tbl = 0 = size tbl

  let equal a b =
    let keys_a = keys a
    in if not (PredicateSet.equal keys_a (keys b))
      then false
      else let domain_matches key =
	     PredicateSet.equal (lookup a key) (lookup b key)
	   in PredicateSet.for_all domain_matches keys_a

  let show table =
    let show_one key value tail =
      ((Predicate.show (key)) ^ ": " ^ (PredicateSet.show value)) :: tail
    in let elts = Hashtbl.fold show_one table []
       in "{| " ^ (String.concat ", " elts) ^ " |}"
end

(* Signals success when searching justification for stratification failure *)
exception StratificationFailure_FoundOne of (Predicate.t *  Rule.t) list
type psr_trace = (PredicateSet.t * Rule.t * (Predicate.t * Rule.t) list)

let compute_rule_dependencies (ruleset) =
  let stratification_failed_for (failures : Predicate.t list) =
    let pset_rules = List.map (function rule -> Rule.body_nonnegativised_predicates (rule), rule) (ruleset) in
    let psr_map = Hashtbl.create (List.length (pset_rules)) in
    let () = begin
      let add_rule ((_, ((p, _), _)) as psr_rule) =
	Hashtbl.add (psr_map) (Predicate.non_negative p) psr_rule
      in List.iter add_rule pset_rules;
      (* let print_x p (ps, r) = *)
      (* 	Printf.eprintf "  %s: %s, %s\n%!" (Predicate.show (p)) (PredicateSet.show (ps)) (Rule.show r) *)
      (* in Hashtbl.iter print_x psr_map *)
    end in

      (* let show_reason (p, rule) = Printf.sprintf "<%s in %s>" (Predicate.show p) (BaseRule.show rule) in *)
      (* let show_trace (prs) = "[" ^ (String.concat "; " (List.map (show_reason) (prs))) ^ "]" in *)
      (* let show_psr (ps, r, trace) = "  (" ^ (PredicateSet.show ps) ^ ", " ^ (Rule.show r) ^ ", " ^ (show_trace trace) ^ ")" in *)
      (* let show_psrs (psrs) = String.concat "\n" (List.map show_psr psrs) in *)

    let explain_failure (base_error_predicate) =
      let base_error_predicate = Predicate.non_negative (base_error_predicate) in
      (* first step: *)
      let candidate_rules =
	let psrs = Hashtbl.find_all (psr_map) (base_error_predicate)
	in List.map (function (ps, r) -> (ps, r, [])) (psrs) in
      (* Our rule choices have type (pset * rule * (predicate * rule) list) *)

      (* BFS step: *)
      let search_forward (psrs : psr_trace list) : psr_trace list =
	let expand_psr (tl : psr_trace list) ((ps, r, trace) : psr_trace) : psr_trace list =
	  if PredicateSet.contains ps base_error_predicate
	  then raise (StratificationFailure_FoundOne ((base_error_predicate, r) :: trace))
	  else let new_choices (pred) (tail) : psr_trace list =
		 let new_psrs = Hashtbl.find_all (psr_map) (pred) in
		 let add_new_psr_to_top (pset, rule) = (pset, rule, (pred, r)::trace) in
		 let new_results = (List.map (add_new_psr_to_top) (new_psrs))
		 in begin
		   (* Printf.eprintf "  %s yields %s\n%!" (Predicate.show pred) (show_psrs new_results); *)
		   new_results @ tail
		 end
	       in begin
		 (* Printf.eprintf "  %s not in %s, so expanding\n%!" (Predicate.show base_error_predicate) (PredicateSet.show ps); *)
		 PredicateSet.fold (new_choices) (ps) (tl)
	       end
	in List.fold_left (expand_psr) ([] : psr_trace list) (psrs)

      (* in *)
      (* let () = Printf.eprintf "Searching for justification for %s\n%!" (Predicate.show base_error_predicate) *)

      (* Search: *)
      in let trace =
	   let rec search_forever (psrs) =
	     begin
	       (match psrs with
		 []	-> failwith "Internal error: could not find justification for stratification failure"
	       | _	-> ());
	       (* Printf.eprintf "Next trace = %s\n%!" (show_psrs psrs); *)
	       search_forever (search_forward (psrs))
	     end
	   in try
		search_forever (candidate_rules)
	     with StratificationFailure_FoundOne (trace) -> trace
	 in Errors.StratificationFailed (base_error_predicate, List.rev (trace))
    in raise (Errors.ProgramError (List.map explain_failure (failures)))
  in
  let table_new () = DepTable.create (List.length (ruleset))
  in
  let illegal_reads = table_new () in (* p(X) :- ~q(X)  means that PredicateSet.contains (DepTable.lookup (illegal_reads, "q")) "p" *)
  let all_defs = table_new () in
  let all_uses = table_new () in
  let new_defs = table_new () in
  let new_uses = table_new ()
  in
  let add def_tbl use_tbl head_p body_ps =
    let add_one_use_dependency (definer : predicate) = DepTable.insert def_tbl definer head_p
    in begin
      DepTable.insert_all use_tbl head_p body_ps;
      PredicateSet.iter add_one_use_dependency body_ps
    end
  in
  let insert_base (((predicate, _), _) as rule) =
    let body_predicates = Rule.body_nonnegativised_predicates (rule) in
    let body_neg_predicates = Rule.body_neg_predicates (rule)
    in begin
      add new_defs new_uses predicate body_predicates;
      PredicateSet.iter (function neg_predicate -> DepTable.insert illegal_reads neg_predicate predicate) (body_neg_predicates)
    end
  in (* initialise new_defs and new_uses *)
  let () = List.iter insert_base ruleset
  in (* initial set of dependencies computed, now compute closure *)
  let rec add_defs_uses (defs) =
    if DepTable.is_empty (defs)
    then ()
    else let new_defs = table_new () in
	 let new_uses = table_new () in
	 let add_new ((all_tbl, all_co_tbl), (new_tbl, new_co_tbl)) predicate new_elements =
	   let really_new = PredicateSet.diff new_elements (DepTable.lookup all_tbl predicate)
	   in begin
(*	     Printf.printf "  Trying to add <%s : %s> to %s\n" (Predicate.show predicate) (PredicateSet.show new_elements) (DepTable.show all_tbl);*)
	     if not (PredicateSet.is_empty really_new)
	     then begin
(*	       Printf.printf "    Adding %s : %s...\n" (Predicate.show predicate) (PredicateSet.show new_elements);*)
	       add all_co_tbl all_tbl predicate really_new;
	       add new_co_tbl new_tbl predicate really_new;
	     end
	   end
	 in
	 let add_all (base) (new_data) =
	   begin
	     DepTable.iter (add_new base) new_data
	   end
	 in let transitive_defs (delta_defs) =
	      let tdefs = table_new() in
	      let add read writes =
		let add_write write =
		  let add_to transitive_write =
		    DepTable.insert tdefs read transitive_write
		  in PredicateSet.iter (add_to) (DepTable.lookup all_defs write)
		in PredicateSet.iter add_write writes
	      in begin
		DepTable.iter add delta_defs;
		tdefs
	      end
	 in begin
(*	   Printf.printf "Iterating add_defs_uses(%s)...\n" (DepTable.show defs);*)
	   add_all ((all_defs, all_uses), (new_defs, new_uses)) defs;
(*	   Printf.printf "=> New entries_uses(%s, %s)...\n" (DepTable.show new_defs) (DepTable.show new_uses);*)
	   let tdefs = transitive_defs (new_defs) in
	   begin
(*	     Printf.printf "=> New entries(%s, %s) => one-step expand = %s...\n" (DepTable.show new_defs) (DepTable.show new_uses) (DepTable.show tdefs);*)
	     add_defs_uses (tdefs) (* tail recurse *)
	   end
	 end
  in begin
(*    Printf.printf "Init adding...\n";*)
    add_defs_uses (new_defs);
    let failures = ref [] in
    let check_broken_predicate pred _ =
      if PredicateSet.contains (DepTable.lookup (all_uses) (Predicate.non_negative pred)) (Predicate.non_negative pred)
      then begin
(*	Printf.eprintf "  Predicate %s depends illegally on itself\n%!" (Predicate.show pred);*)
	failures := pred :: !failures
      end
      in begin
	DepTable.iter check_broken_predicate illegal_reads;
	match !failures with
	  []	-> ()  (* all could be stratified *)
	| _	-> ignore (stratification_failed_for (!failures));
      end;
    (* result: *)
    (all_defs, all_uses)
  end

type def_use_stratum = { preds : PredicateSet.t;
			 defs  : PredicateSet.t;
			 uses  : PredicateSet.t }

module DefUseStratum =
  struct
    type t = def_use_stratum

    let equal (stratum0) (stratum1) =
      let eq = PredicateSet.equal
      in (eq (stratum0.preds) (stratum1.preds)
	  && eq (stratum0.defs) (stratum1.defs)
	  && eq (stratum0.uses) (stratum1.uses))

    let rec equal_strata (strata0) (strata1) =
      match (strata0, strata1) with
	  ([], [])	-> true
	| (a::a_s,
	   b::b_s)	->
	  if equal a b
	  then equal_strata a_s b_s
	  else false
	| _		-> false (* length mismatch *)

    let equal_strata_with_set_semantics (strata0) (strata1) =
      (* compare in the sense that all individual entries are contained in the other list *)
      let contained_in others entry = List.exists (equal entry) others in
      let all_contained_in a b = List.for_all (contained_in a) b
      in all_contained_in strata0 strata1 && all_contained_in strata1 strata0

    let show { preds; defs; uses } =
      let s = PredicateSet.show
      in "{< " ^ (s preds) ^ ": d=" ^ (s defs) ^ "; u=" ^ (s uses) ^ " >}"
  end


let equivalence_clusters (defs, uses) (predicate_set) =
  let cluster_map = DepTable.create (PredicateSet.cardinal (predicate_set))
  in let clusters : def_use_stratum list ref = ref []
     in
     let encluster (var) =
       if PredicateSet.is_empty (DepTable.lookup cluster_map var)
       then let cluster = let proto_cluster = PredicateSet.inter (DepTable.lookup defs var) (DepTable.lookup uses var)
			  in
			  if PredicateSet.is_empty proto_cluster
			  then PredicateSet.singleton var
			  else proto_cluster
	    in let add_cluster_item (key) = DepTable.insert_all cluster_map key cluster
	       in begin
		 PredicateSet.iter add_cluster_item cluster;
		 clusters := { preds = cluster; defs = DepTable.lookup defs var; uses = DepTable.lookup uses var } :: !clusters
	       end
       else ()
     in begin
       PredicateSet.iter encluster predicate_set;
       !clusters
     end

let sort_equivalence_clusters (strata : (* unsorted *) def_use_stratum list) =
  let compare (stratum0) (stratum1) =
    if PredicateSet.equal stratum0.preds stratum1.preds
    then 0
    else if not (PredicateSet.is_empty (PredicateSet.inter stratum1.uses stratum0.defs))
    then -1 (* stratum1 depends on stratum0, so stratum0 should be left*)
    else 1
  in List.sort compare strata

let predicate_set_from_ruleset (ruleset) =
  List.fold_left (function set -> function ((p, _), _) -> PredicateSet.add p set) PredicateSet.empty ruleset

let make_stratum (ruleset) (pss) =
  let literal_is_relevant (p, _)	= PredicateSet.contains pss p in
  let gen_stratum (snstratum) ((head, body) as rule : rule) =
    if not (literal_is_relevant (head))
    then snstratum
    else let rec find_deltas (body_prefix, body_tail) =
	   match body_tail with
	       []	-> []
	     | h::tl	->
	       if literal_is_relevant h
	       then ((*Literal.delta*) (head), [Literal.delta(h)] @ List.rev (body_prefix) @ tl) :: find_deltas (h::body_prefix, tl)
	       else find_deltas (h::body_prefix, tl)
	 in let delta_rules = find_deltas ([], body)
	    in {
	      Stratum.pss	= snstratum.pss;
	      Stratum.base	= rule :: snstratum.base;
	      Stratum.delta	= delta_rules @ snstratum.delta
	    }
  in List.fold_left gen_stratum { pss = pss; base = []; delta = [] } ruleset

let stratify (ruleset: ruleset) =
    (* (\* #1: Ground the rules *\) *)
    (* let ruleset = map ground_rule ruleset in *)
    (* #2: dependencies *)
  let (defs, uses) = compute_rule_dependencies (ruleset) in
    (* #3: cluster *)
  let predicate_set = predicate_set_from_ruleset (ruleset) in
  let unsorted_cluster_list = equivalence_clusters (defs, uses) (predicate_set) in
    (* #4 stratify *)
  let strata = sort_equivalence_clusters (unsorted_cluster_list) in
    (* #5 add delta rules for semi-naive evaluation, constructing a stratified_ruleset *)
  let stratum_predicate_sets = List.map (function s -> s.preds) strata
  in
  List.map (make_stratum ruleset) stratum_predicate_sets

