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

 The author can be reached as "creichen" at "gmail.com".

***************************************************************************)

open Base
open Program
module DB = Database
module PrimopInterface = Primop_interface

module DBFrontend =
struct
  let create () =
    DB.create (Combined_table.create)

  (* EDB insertion *)
  let add db ((predicate, ((_, atoms) as tuple)) : fact) =
    let insert pred (tuple) = Combined_table.insert (DB.get_table db pred) tuple
    in begin
      insert (Predicate.P predicate) tuple;
      Array.iter (function v -> insert Predicate.atom ([|Label.none|], [|v|])) atoms
    end

  (* EDB removal *)
  let remove db ((predicate, tuple) : fact) =
    Combined_table.remove (DB.get_table db (Predicate.P predicate)) tuple

  let import db (facts : fact list) =
    List.iter (add db) facts
end


module ProgramFrontend =
struct
  type t = {
    mutable rules : Rule.t list;
    signatures : (Predicate.t, Signature.t) Hashtbl.t;
    mutable stratified_program : stratum list option;
  }

  let create () = {
    rules = [];
    signatures = Hashtbl.copy (PrimopInterface.primops_signatures);
    stratified_program = None;
  }

  let signatures (program) =
    let cc lhs rhs tail = (lhs, rhs) :: tail
    in Hashtbl.fold (cc) program.signatures [] 

  let get_signature (program) (pred) =
    Hashtbl.find program.signatures pred

  (* Performs semantic analysis of a rule wrt a program, as well as access path selection *)
  let semantic_check (errlist : Errors.error list ref) (program) ((head, bodies) as rule) =
    let had_error = ref false in
    let check_literal (p, (labels, variables)) =
      let errflag = ref false in
      let err (error) =
	begin
	  had_error := true;
	  errflag := true;
	  errlist := error :: !errlist;
	end
      in begin
	(try
	   Signature.check_positional_after_nominal (labels)
	 with
	   Signature.Positional_after_nominal (label) -> err (Errors.PositionalAfterNominal (p, label, rule)));
	if not !errflag
	then begin
	  Label.order_labels (variables) (labels);
	  try
	    let signat = Signature.from_labels (labels)
	    in try if Hashtbl.find (program.signatures) (p) <> signat
	      then err (Errors.SignatureMismatch (p, Hashtbl.find program.signatures p, signat, rule))
	      with
		Not_found -> Hashtbl.add program.signatures (p) (signat)
	  with
	    Signature.Duplicate_label (label) -> err (Errors.DuplicateLabel (p, label, rule))
	end
      end

    in begin
      check_literal head;
      List.iter (check_literal) bodies;
      if (!had_error)
      then None
      else Some (Rule.normalise (rule))
    end

  (* internal use only *)
  let some_adding (adder) (program : t) (entity_to_add) =
    let errors = ref []
    in begin
      adder (errors) (program) (entity_to_add);
      program.stratified_program <- None;  (* insertion may have invalidated stratification *)
      match !errors with
	[]	-> ()
      | _	-> raise (Errors.ProgramError (!errors))
    end

  (* internal use only *)
  let add_unsafe (errors) (program : t) (rule : Rule.t) =
    match semantic_check (errors) (program) (rule) with
      None	-> ()
    | Some r	-> program.rules <- r :: program.rules

  let add = some_adding (add_unsafe)

  let import =
    let do_add_multi (errors) (program : t) (rules : Rule.t list) =
      List.iter (add_unsafe (errors) (program)) rules
    in some_adding (do_add_multi)

  let singleton (rule) =
    let program = create()
    in begin
      add (program) rule;
      program
    end

  let rules (program) = List.rev (program.rules)

  let stratify (program : t) =
    begin
      if not (Option.is_some program.stratified_program)
      then program.stratified_program <- Some (Stratification.stratify (program.rules))
    end

  let eval (program : t) (db) =
    begin
      stratify (program);
      Eval.eval (db) (Option.value_of (program.stratified_program));
    end
end
