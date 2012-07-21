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

module BaseRule = Base_rule

type error =
  ParseError of ((* line *) int * (* column *) int) * (* message *) string
| NoAccessPath of BaseRule.t
| StratificationFailed of Predicate.t * (Predicate.t * BaseRule.t) list (* starting with a rule that has the predicate in the rhs, ending with it in the lhs *)

exception ProgramError of error list

let all_equal (aerrors) (berrors) = (Sort.list (>) aerrors) = (Sort.list (>) berrors)

let equals (aerrors) (berrors) =
  match (aerrors, berrors) with
    (ProgramError alist, ProgramError blist)	-> all_equal (alist) (blist)
  | _						-> false

let show_pos (line, offset) = Printf.sprintf "L%d %d" line offset

let show_error (error) =
  match error with
    ParseError (pos, message)		-> Printf.sprintf "%s: parse error: %s\n" (show_pos pos) message
  | NoAccessPath (rule)			-> Printf.sprintf "Error in rule %s:\nCould not find viable access path" (BaseRule.show (rule))
  | StratificationFailed (pred,
			  derivation)	-> let predlen p = String.length (Predicate.show p) in
					   let max_predlen =
					     let comp plen (pred, _) =
					       let plen' = predlen pred
					       in max plen' plen
					     in List.fold_left comp 0 derivation in
					   let show_pred p =
					     let rec pad i = if i = 0
					       then ""
					       else " " ^ (pad (i-1)) in
					     let predstr = Predicate.show p
					     in predstr ^ (pad (max_predlen - (String.length predstr)))
					   in
					   let show_reason (p, rule) = Printf.sprintf "  %s in %s" (show_pred p) (BaseRule.show rule)
					   in Printf.sprintf "Stratification failed:  predicate `%s' must be stratified but depends on itself via:\n%s"
					         (Predicate.show pred)
						 (String.concat "\n" (List.map show_reason derivation))

let show_errors (errlist) =
  (String.concat "\n" (List.map show_error errlist))
  ^ (if List.length (errlist) > 1
    then Printf.sprintf"\n%d errors total.\n" (List.length errlist)
    else "")


module Parser =
  struct
    let msg_primop_in_head (primop_name) =
      "Primitive operator `" ^ primop_name ^ "' in rule head"

    let msg_negative_head (literal_str) =
      "Negative literal `" ^ literal_str ^ "' in rule head"
  end
