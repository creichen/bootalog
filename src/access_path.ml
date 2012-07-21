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
open Dynamic_programming
module PI = Primop_interface

let access_path_buffer_size = 16  (* more slows down access path selection but may yield better results *)

module Conf =
struct
  type action = int * PI.access_cost * literal
  type state = VarSet.t * (literal list)
  type cost = PI.access_cost

  let all_actions ((varset, list) : state) =
    let rec gen i list =
      match list with
	[]	-> []
      | h::tl	->
	begin
	  match Literal.link_and_get_access_cost (varset) (h) with
	    None	-> gen (i+1) tl
	  | Some (literal,
		  cost)	-> (i, cost, literal)::(gen (i+1) tl)
	end
    in gen 0 list

  let apply_action ((vars, literal_list) : state) (action_index, _, literal) =
    let rec remove i list =
      match list with
	[]	-> failwith "internal action application error"
      | h::tl	->
	if i = 0
	then tl
	else h::(remove (i-1) tl)
    in let new_literal_list = remove action_index literal_list in
       let new_vars = (* for literals, we don't bind any variables *)
	 if Literal.is_neg literal
	 then vars
	 else VarSet.union (Literal.vars (literal)) (vars)
       in (new_vars, new_literal_list)

  let is_success_state (_, list) = list = []

  let cost (_, cost, _) = cost

  let zero_cost = PI.zero_cost
  let combine_cost = PI.combine_cost
  let compare_cost = PI.compare_cost

  let estimated_completion_cost (varset, literals_list) =
    List.fold_right (function element -> combine_cost (Literal.estimate_access_cost varset element))
                    literals_list
                    zero_cost

  let show_state (vars, literals) = Printf.sprintf "%s:[%s]" (VarSet.show (vars)) (String.concat "," (List.map Literal.show literals))
  let show_action (action_index, cost, literal) = Printf.sprintf "#%d:%s:%s" (action_index) (PI.show_cost cost) (Literal.show literal)
  let show_cost = PI.show_cost
end

module AccessPathSelector = DP(Conf)

(* Access path selection function.
   Uses dynamic programming to find cheapest access path. *)
let select ((head, tail) as rule : rule) =
    let head_vars = VarSet.empty (*BaseRule.head_vars (rule)*) in
    let initial_state = (head_vars, tail) in
    let result = AccessPathSelector.solve (access_path_buffer_size) (initial_state) in
    let extract (_, _, literal) = literal
    in match result with
      Some new_tail	-> (head, List.map extract new_tail)
    | None		-> raise (Errors.ProgramError [Errors.NoAccessPath rule])
