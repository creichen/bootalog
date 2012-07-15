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
open Frontend
open Program.Lexeme
open OUnit
module PI = Primop_interface

let get_primop_evaluator primop_name vmodes =
  let pid = Primops.resolve primop_name in
  let amodes = Primops.get (pid) in
  let amode = List.hd (PI.access_modes vmodes amodes)
  in (primop_name ^ "[" ^ (String.concat "" (List.map PI.show_variable_mode vmodes)) ^ "]",
      amode.PI.evaluator)

let test_eval' primop_name test_name vmodes input_tuple output_tuples =
  let resolved_name, evaluator = get_primop_evaluator primop_name vmodes in
  let full_test_name = resolved_name ^ (if "" = test_name then "" else "-" ^ test_name) in
  let env = Env.fresh () in
  let output_extractor = ref [] in
  let args =
    let rec aname i vm_l input_l =
      match (vm_l, input_l) with
	([], [])	-> []
      | (PI.Bound::vmtl,
	 input::inputl)	-> let var = Printf.sprintf "I%d" i
			   in begin
			     Env.bind env var input;
			     var :: aname (i+1) vmtl inputl
			   end
      | (PI.Free::vmtl,
	 _) 		-> let var = Printf.sprintf "O%d" i
			   in begin
			     output_extractor := var :: !output_extractor;
			     var :: aname (i+1) vmtl input_l
			   end
      | _		-> failwith "vmodes and input_tuple disagree in relevant length"
    in aname 0 vmodes input_tuple in
  let show_result result = "[" ^ (String.concat ", " result) ^ "]" in
  let show_results results = "< " ^ (String.concat " ; "  (List.map show_result results)) ^ " >" in
  let body = Array.of_list args in
  let outputs = List.rev (!output_extractor) in
(*
  let () = begin
    Printf.eprintf "  %s: env = %s; args = %s\n%!" (full_test_name) (Env.show env) (show_result args)
  end in *)
  let get_output (env') =
    let get var =
      match Env.lookup env' var with
	Some value	-> value
      | None		-> failwith ("Output variable `" ^ var ^ "' not bound")
    in List.map (get) outputs in
  let test () =
    let outputs = ref [] in
    let register_output (e) = begin
(*      Printf.eprintf "    -> env = %s; => %s\n%!" (Env.show e) (show_result (get_output(e)));*)
      outputs := (get_output (e)) :: !outputs
    end
    in begin
      evaluator (body) (env) (register_output);
      let actuals = show_results (List.rev !outputs) in
      let expecteds = show_results (output_tuples)
      in assert_equal actuals expecteds ?msg:(Some (Printf.sprintf "Mismatch:\nactual  : %s\nexpected: %s" (actuals) (expecteds)))
    end
  in full_test_name >:: test

let test_eval opname = test_eval' opname "" 

let success = [[]]
let failure = []

let bb = PI.Bound
let ff = PI.Free

let all_tests = "access-modes" >:::
  [
    test_eval "=" [bb; ff] ["42"] [["42"]];
    test_eval "=" [ff; bb] ["42"] [["42"]];
    test_eval' "=" "match" [bb; bb] ["42"; "42"] success;
    test_eval' "=" "mismatch" [bb; bb] ["42"; "23"] failure;
  ]

let _ = run_test_tt_main (all_tests)
