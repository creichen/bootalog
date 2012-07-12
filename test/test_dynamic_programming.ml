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

open OUnit
open Dynamic_programming

(*

Our first batch of tests operates on the following AI problem:

Assume an infinite 2D grid game board and a single game piece.
Assume further a set of moves with associated cost.

In a fixed number of steps, move the game piece as close to (0, 0).
Minimise: aggregate move cost + your Manhattan distance to (0, 0).

*)

type pos = int * int

let moves = ref []

module Conf =
  struct
    type action = int (* jump cost; <= distance here *) * (int * int) (* deltas on the position *)
    type state = int (* steps to go *) * pos
    type cost = int (* Manhattan distance to zero *)

    let all_actions _ = !moves
    let apply_action (steps, (x, y)) (_, (dx, dy)) = (steps - 1, (x + dx, y + dy))
    let is_success_state (steps, (x, y)) = steps = 0 || (x = 0 && y = 0)

    let estimated_completion_cost (_, (x, y)) = (abs x) + (abs y)
    let cost (v, _) = v

    let zero_cost = 0
    let combine_cost (a) (b) = a + b

    let compare_cost a b = a - b

    let show_state (c, (x, y)) = Printf.sprintf "<%d, (%d, %d)>" c x y
    let show_action (c, (x, y)) = Printf.sprintf "%d#(%d, %d)>" c x y
    let show_cost cost = Printf.sprintf "%d" cost
  end

module Solver = DP(Conf)


let is_permutation_of arr0 arr1 =
  let cmp (a, (b, c)) (a', (b', c')) =
    let mr x x' next =
      if x == x' then next
      else x < x'
    in mr a a' (mr b b' (mr c c' true))
  in Sort.list cmp arr0 = Sort.list cmp arr1

let show_result result =
  match result with
    None	-> "none"
  | Some l	-> "[" ^ (String.concat ", " (List.map Conf.show_action l)) ^ "]"

let check_commutative_solution buf_size steps_nr startpos allowed_moves expected () =
  begin
    moves := allowed_moves;
    let answer = Solver.solve (buf_size) (steps_nr, startpos) in
    let message = Printf.sprintf "Solutions mismatch (%d steps, buffer size %d):\nexpected: %s\nactual  : %s\nstart configuration: %s\nmoves: %s\n"
      steps_nr buf_size (show_result expected) (show_result answer) (Conf.show_state (steps_nr, startpos)) (show_result (Some allowed_moves)) in
    let cmp_result a b =
      match (a, b) with
	None, None	-> true
      | Some a, Some b	-> is_permutation_of a b
      | _		-> false

    in assert_equal true (cmp_result answer expected) ?msg:(Some message)
  end

let show_buf buf =
  let show_dp_state' s =
    match s with
      None	-> "-"
    | Some s	-> Solver.show_dp_state s
  in Printf.sprintf "[|%s|]" (String.concat ", " (List.map show_dp_state' (Array.to_list buf)))

open Solver

let check_buffer_insert initial_buffer initial_buffer_size new_element expected_buffer expected_buffer_size () : unit =
  (* let () = Printf.eprintf "Checking A\n%!" in *)
  let show_buf buf = (* simplified version *)
    let show_dp_state' s =
      match s with
  	None	-> "-"
      | Some s	-> Printf.sprintf "%d" s.estimated_cost
    in Printf.sprintf "[|%s|]" (String.concat ", " (List.map show_dp_state' (Array.to_list buf)))
  in let initial_buffer_backup = Array.copy initial_buffer in
     let buf_size = ref initial_buffer_size in
     (* let () = Printf.eprintf "Checking B\n%!" in *)
     let () = Solver.insert_into_buffer (buf_size) (initial_buffer) (new_element) in
     (* let () = Printf.eprintf "Checking C\n%!" in *)
     let message = ((Printf.sprintf "Mismatch on inserting %d:\n" new_element.estimated_cost)
  		    ^ " starting from: " ^ (show_buf initial_buffer_backup) ^ (Printf.sprintf "  (size %d)\n" initial_buffer_size)
  		    ^ " we obtained  : " ^ (show_buf initial_buffer) ^ (Printf.sprintf "  (size %d)\n" (!buf_size))
  		    ^ " but expected : " ^ (show_buf expected_buffer) ^ (Printf.sprintf "  (size %d)\n" (expected_buffer_size)))
     in begin
       assert_equal (initial_buffer) (expected_buffer) ?msg:(Some message);
       assert_equal (!buf_size) (expected_buffer_size) ?msg:(Some message)
     end

let check_buffer_size_n n () =
  let m_example cost =
    { estimated_cost = cost;
      cost_so_far = 0;
      actions = [];
      state = (cost, (0, 0)) }
  in let check_fill_level fill_level =
       for insertion_index = 0 to fill_level do
	 let new_value = m_example (insertion_index * 2) in
	 let result_array_size =
	   if fill_level = n
	   then fill_level
	   else fill_level + 1
	 in
	 let base_array = Array.create n None in
	 let expected_array = Array.create n None in
	 begin
	   for k = 0 to fill_level - 1 do
	     let elt = Some (m_example (1 + k * 2))
	     in begin
	       Array.set base_array k elt;
	       let expected_insertion_point = if k < insertion_index then k else k + 1
	       in if expected_insertion_point < n then Array.set expected_array expected_insertion_point elt
	     end
	   done;
	     (* Fill the a hole in the expected_array, unless we were adding beyond the end: *)
	   if insertion_index < n
	   then Array.set expected_array insertion_index (Some new_value);
	     (* Now check: *)
	   check_buffer_insert base_array fill_level new_value expected_array result_array_size ()
	 end
       done
     in for z = 0 to n do
	 check_fill_level z
       done

let m cost =
  { estimated_cost = cost;
    cost_so_far = 0;
    actions = [];
    state = (cost, (0, 0)) }

let sm cost =
  Some (m cost)


let left n = (n - 1, (- n, 0))
let right n = (n - 1, (n, 0))

let up n = (n - 1, (0, - n))
let down n = (n - 1, (0, n))

let all_tests = "dynamic-programming" >:::
  [
    "is-bit" >:: (function () -> assert_equal [0;1;2;4;8;16] (List.filter is_bit [0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20]));

    "insert-1.0" >:: (check_buffer_insert
    			[| None |] 0
    			(m 1)
    			[| sm 1 |] 1);
    "insert-1.1" >:: (check_buffer_insert
    			[| sm 2 |] 1
    			(m 1)
    			[| sm 1 |] 1);
    "insert-1.2" >:: (check_buffer_insert
    			[| sm 0 |] 1
    			(m 1)
    			[| sm 0 |] 1);

    "insert-2.0" >:: (check_buffer_insert
    			[| sm 2; None |] 1
    			(m 1)
    			[| sm 1; sm 2 |] 2);
    "insert-2.1" >:: (check_buffer_insert
    			[| sm 0; None |] 1
    			(m 1)
    			[| sm 0; sm 1 |] 2);
    "insert-2.2" >:: (check_buffer_insert
    			[| sm 1; None |] 1
    			(m 1)
    			[| sm 1; sm 1 |] 2);

    "insert-3.0" >:: (check_buffer_insert
    			[| None; None; None |] 0
    			(m 1)
    			[| sm 1; None; None; |] 1);
    "insert-3.1" >:: (check_buffer_insert
    			[| sm 0; sm 2 ; sm 4 |] 3
    			(m 1)
    			[| sm 0; sm 1; sm 2 |] 3);
    "insert-3.1" >:: (check_buffer_insert
    			[| sm 2; sm 3 ; sm 4 |] 3
    			(m 1)
    			[| sm 1; sm 2; sm 3 |] 3);
    "insert-3.1" >:: (check_buffer_insert
    			[| sm 2; sm 3 ; sm 5 |] 3
    			(m 4)
    			[| sm 2; sm 3; sm 4 |] 3);

    "insert-1" >:: check_buffer_size_n 1;
    "insert-2" >:: check_buffer_size_n 2;
    "insert-3" >:: check_buffer_size_n 3;
    "insert-4" >:: check_buffer_size_n 4;
    "insert-5" >:: check_buffer_size_n 5;
    "insert-6" >:: check_buffer_size_n 6;
    "insert-7" >:: check_buffer_size_n 7;
    "insert-8" >:: check_buffer_size_n 8;

    "solve-commutative-alpha-0" >:: check_commutative_solution 1
      (* rounds: *)   2
      (* startpos: *) (5, 0)
      (* moves: *)    [left 1; right 1; up 1; down 1]
      (Some [left 1; left 1]);

    "solve-commutative-alpha-1" >:: check_commutative_solution 1
      (* rounds: *)   3
      (* startpos: *) (2, 1)
      (* moves: *)    [left 1; right 1; up 1; down 1]
      (Some [left 1; left 1; up 1]);

    (let expensive_left = (3, (-1, 0)) in
     let expensive_right = (3, (1, 0)) in
     let jump_lu = (1, (-3, -7)) in
     "solve-commutative-beta-0" >:: check_commutative_solution 3
      (* rounds: *)   3
      (* startpos: *) (3, 0)
      (* moves: *)    [expensive_left; expensive_right; jump_lu; down 3]
       (Some [jump_lu; down 3; down 3]));

    (let jump_1 = (1, (7, 3)) in
     let jump_2 = (1, (-4, 1)) in
     let jump_3 = (1, (1, -5)) in
     "solve-commutative-gamma-0" >:: check_commutative_solution 3
      (* rounds: *)   4
      (* startpos: *) (-5, 6)
      (* moves: *)    [jump_1; jump_2; jump_3]
      (Some [jump_1; jump_2; jump_3; jump_3]));
  ]


let _ = run_test_tt_main (all_tests)

