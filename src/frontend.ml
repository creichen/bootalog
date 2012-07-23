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
    mutable stratified_program : stratum list option;
  }

  let create () = {
    rules = [];
    stratified_program = None;
  }

  (* Performs semantic analysis of a rule wrt a program, as well as access path selection *)
  let semantic_check (_program) (rule) =
    let rule = Rule.normalise (rule)
    in rule

  let add (program : t) (rule : Rule.t) =
    begin
      program.rules <- (semantic_check (program) (rule)) :: program.rules;
      program.stratified_program <- None;  (* insertion may have invalidated stratification *)
    end

  let import (program) (rules) =
    List.iter (add program) rules

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
