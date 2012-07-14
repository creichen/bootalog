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

module type DP_SIG =
  sig
    type state
    type action
    type cost

    val show_state : state -> string
    val show_action : action -> string
    val show_cost : cost -> string

    (* expand all possible actions from given state *)
    val all_actions : state -> action list
    val apply_action : state -> action -> state
    val is_success_state : state -> bool

    (* estimated cost of finishing from this state *)
    val estimated_completion_cost : state -> cost
    val cost : action -> cost

    val zero_cost : cost
    val combine_cost : cost -> cost -> cost

    (* negative if lhs is cheaper *)
    val compare_cost : cost -> cost -> int
  end

let is_bit x = ((x - 1) land (lnot x)) + 1 = x

module DP = 
  functor (M : DP_SIG) ->
  struct
    open M

    type dp_state =
      { estimated_cost : cost;
	cost_so_far    : cost;
	actions	       : action list;
	state	       : state }

    let show_dp_state (dp_state) =
      Printf.sprintf "[%s (so far: %s); current:[%s] to-do:%s]"
	(show_cost dp_state.estimated_cost)
	(show_cost dp_state.cost_so_far)
	(String.concat ", " (List.map show_action dp_state.actions))
	(show_state dp_state.state)

    let show_dp_state' s =
      match s with
  	None	-> "-"
      | Some s	-> (show_dp_state s)

    let show_buf buf = (* simplified version *)
      Printf.sprintf "[|%s|]" (String.concat ", " (List.map show_dp_state' (Array.to_list buf)))


    let insert_into_buffer (results_nr : int ref) (buffer : dp_state option array) (entry : dp_state) =
      let buffer_size = Array.length buffer in
      let estimated_cost_at i : cost = let Some v = Array.get buffer i in v.estimated_cost in
      let insert_at offset =
	if offset < buffer_size
	then begin
	  if !results_nr < buffer_size
	  then results_nr := 1 + !results_nr;
	  if offset < !results_nr
	  then Array.blit buffer offset
	                  buffer (offset + 1) (!results_nr - offset - 1);
	  Array.set buffer offset (Some entry)
	end in

      let bound_div_2 =
	let rec find_it x = if is_bit x then x else find_it (x-1)
	in if !results_nr = 0 then 0 else find_it (!results_nr)
      in
      let rec find_entry pos stride =
	let cmp =
	  if pos >= !results_nr
	  then -1
	  else compare_cost (entry.estimated_cost) (estimated_cost_at pos)
	in if stride = 0
	  then Some (if cmp <= 0 then pos else pos + 1)
	  else if cmp < 0
	  then find_entry (pos - stride) (stride lsr 1)
	  else find_entry (pos + stride) (stride lsr 1)
      in
      let start_point = find_entry (if bound_div_2 = 0 then 0 else bound_div_2 - 1) (bound_div_2 lsr 1)
      in match start_point with
	None	-> if !results_nr < buffer_size then insert_at (!results_nr)
      | Some i	-> insert_at i

    (* Assume that earlier solutions always win.  Otherwise, this is not always optimal. *)
    (* Optimised for buffer sizes that aren't terribly huge, since we're blitting over an array of that size. *)
    let solve (buffer_size : int) (initial_state : state) =
      let initial_buffers = [|Some { estimated_cost	= zero_cost;
				     cost_so_far	= zero_cost;
				     actions		= [];
				     state		= initial_state }|] in
      if is_success_state (initial_state)
      then Some []
      else
      let iteration_nr = ref 0 in
      let print_state (array) (entries_nr) =
	begin
	  Printf.eprintf "State #%d:\n" (!iteration_nr);
	  for i = 0 to entries_nr - 1 do
	    Printf.eprintf "  %s\n" (show_dp_state' (Array.get array i))
	  done;
	  Printf.eprintf "\n%!"
	end in

      let rec search inputs inputs_nr =
(*	let () = print_state inputs inputs_nr in*)
	let results_nr = ref 0 in
	let results = Array.make buffer_size None in
	let try_action dp_state action =
	  let updated_state = apply_action dp_state.state action in
	  let updated_cost_so_far = combine_cost (dp_state.cost_so_far) (cost action) in
	  let updated_dp_state = {
	    estimated_cost	= combine_cost (updated_cost_so_far) (estimated_completion_cost updated_state);
	    cost_so_far		= updated_cost_so_far;
	    state		= updated_state;
	    actions		= action :: dp_state.actions }
	  in insert_into_buffer (results_nr) (results) (updated_dp_state)
	in
	let try_state dp_state' =
	  match dp_state' with
	    None		-> ()
	  | Some dp_state	->
	    let possible_actions = all_actions dp_state.state
	    in List.iter (try_action dp_state) (possible_actions)
	in begin
	  for i = 0 to inputs_nr - 1 do
	    try_state (Array.get inputs i)
	  done;
(*	  iteration_nr := 1 + !iteration_nr;
	  let () = print_state results (!results_nr) in*)
	  let comp_result = ref None
	  in let check_solution dp_state' =
	       match !comp_result with
		 Some _	-> ()
	       | None	-> (
		 match dp_state' with
		   None			-> ()
		 | Some dp_state	->
		   if is_success_state (dp_state.state)
		   then comp_result := Some (List.rev (dp_state.actions))
	       )
	     in begin
	       for i = 0 to !results_nr - 1 do
		 check_solution (Array.get results i)
	       done;
	       match !comp_result with
		 Some r	-> Some r
	       | None	->
		 if !results_nr > 0
		 then search results (!results_nr) (* continue *)
		 else None (* we have failed *)
	     end
	end
      in search (initial_buffers) (1)
	  
  end
