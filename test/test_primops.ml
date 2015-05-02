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
    in let env_before = Env.show env
    in begin
      evaluator (body) (env) (register_output);
      let actuals = show_results (List.rev !outputs) in
      let expecteds = show_results (output_tuples)
      in begin
	assert_equal actuals expecteds ?msg:(Some (Printf.sprintf "Mismatch:\nactual  : %s\nexpected: %s" (actuals) (expecteds)));
	assert_equal env_before (Env.show env) ?msg:(Some (Printf.sprintf "Environment not restored properly:\nbefore: %s\nafter : %s" (env_before) (Env.show env)));
      end
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

    test_eval' "sys-concat" "match" [bb; bb; bb] ["foo"; "bar"; "foobar"] success;

    test_eval' "sys-concat" "fail-0" [bb; bb; bb] ["foo"; "bar"; "foobarx"] failure;
    test_eval' "sys-concat" "fail-1" [bb; bb; bb] ["foo"; "bar"; "fooxbar"] failure;
    test_eval' "sys-concat" "fail-2" [bb; bb; bb] ["foo"; "bar"; "xfoobar"] failure;
    test_eval "sys-concat" [bb; bb; ff] ["foo"; "bar"] [["foobar"]];
    test_eval "sys-concat" [bb; ff; bb] ["foo"; "foobar"] [["bar"]];
    test_eval "sys-concat" [ff; bb; bb] ["bar"; "foobar"] [["foo"]];
    test_eval "sys-concat" [ff; ff; bb] ["foobar"] [[""; "foobar"]; ["f"; "oobar"]; ["fo"; "obar"]; ["foo"; "bar"]; ["foob"; "ar"]; ["fooba"; "r"]; ["foobar"; ""]];

    test_eval "sys-length" [bb; ff] ["bar"] [["3"]];
    test_eval "sys-length" [bb; bb] ["bar"; "3"] success;

    test_eval "sys-add" [bb; bb; bb] ["2"; "3"; "5"] success;
    test_eval "sys-add" [bb; bb; ff] ["2"; "3"] [["5"]];
    test_eval "sys-add" [bb; ff; bb] ["2"; "25"] [["23"]];
    test_eval "sys-add" [ff; bb; bb] ["2"; "25"] [["23"]];

    test_eval "sys-sub" [bb; bb; bb] ["2"; "3"; "-1"] success;
    test_eval "sys-sub" [bb; bb; ff] ["17"; "3"] [["14"]];
    test_eval "sys-sub" [bb; ff; bb] ["27"; "25"] [["2"]];
    test_eval "sys-sub" [ff; bb; bb] ["2"; "5"] [["7"]];

    test_eval "sys-mul" [bb; bb; bb] ["6"; "7"; "42"] success;
    test_eval "sys-mul" [bb; bb; ff] ["6"; "7"] [["42"]];
    test_eval "sys-mul" [bb; ff; bb] ["10"; "120"] [["12"]];
    test_eval "sys-mul" [ff; bb; bb] ["10"; "120"] [["12"]];

    test_eval "sys-div" [bb; bb; bb] ["14"; "2"; "7"] success;
    test_eval "sys-div" [bb; bb; ff] ["42"; "6"] [["7"]];
    test_eval' "sys-div" "zero" [bb; bb; ff] ["42"; "0"] failure;
    test_eval "sys-div" [bb; ff; bb] ["42"; "7"] [["6"]];
    test_eval' "sys-div" "zero" [bb; ff; bb] ["42"; "0"] failure;
    test_eval "sys-div" [ff; bb; bb] ["6"; "7"] [["42"]];

    test_eval "sys-modulo" [bb; bb; bb] ["23"; "10"; "3"] success;
    test_eval' "sys-modulo" "zero" [bb; bb; bb] ["23"; "0"; "3"] failure;
    test_eval' "sys-modulo" "negative" [bb; bb; bb] ["23"; "-3"; "3"] failure;
    test_eval "sys-modulo" [bb; bb; ff] ["23"; "10"] [["3"]];
    test_eval' "sys-modulo" "zero" [bb; bb; ff] ["23"; "0"] failure;
    test_eval' "sys-modulo" "negative" [bb; bb; ff] ["23"; "-3"] failure;

    test_eval' "sys-lt" "true" [bb; bb] ["22"; "23"] success;
    test_eval' "sys-lt" "bound" [bb; bb] ["23"; "23"] failure;
    test_eval' "sys-lt" "false" [bb; bb] ["24"; "23"] failure;

    test_eval' "sys-le" "true" [bb; bb] ["22"; "23"] success;
    test_eval' "sys-le" "bound" [bb; bb] ["23"; "23"] success;
    test_eval' "sys-le" "false" [bb; bb] ["24"; "23"] failure;

    test_eval' "sys-gt" "false" [bb; bb] ["22"; "23"] failure;
    test_eval' "sys-gt" "bound" [bb; bb] ["23"; "23"] failure;
    test_eval' "sys-gt" "true" [bb; bb] ["24"; "23"] success;

    test_eval' "sys-ge" "false" [bb; bb] ["22"; "23"] failure;
    test_eval' "sys-ge" "bound" [bb; bb] ["23"; "23"] success;
    test_eval' "sys-ge" "true" [bb; bb] ["24"; "23"] success;
  ]

let _ = run_test_tt_main (all_tests)
