(***************************************************************************
 This file is Copyright (C) 2010 Christoph Reichenbach

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

open Getopt
open OUnit
open Printf

let check_output client expected =
  let accumulator = ref "" in
  let pr s = accumulator := !accumulator ^ s in
  begin
    client (pr);
    assert_equal (!accumulator) (expected) ?msg:(Some (sprintf "Expected `%s' but found `%s'" expected !accumulator))
  end

let noarg x = NoArg (fun s -> s @ [x])
let witharg x n = WithArg (n, function (s, s') -> s @ [x ^ ":" ^ s'])

let options = [(None, "l-one", noarg "1", "#1");
	       (None, "l-two", witharg "2" "alpha", "#2");
	       (Some 'a', "l-three", witharg "3" "zeta", "#3");
	       (Some 'b', "l-four", noarg "4", "#4");
	       (Some 'c', "l-five", noarg "5", "#5")]

let test_result (args) expected_result expected_leftovers () =
  let test_result' (actual_result, actual_leftovers) =
    let show_list (list) = "[" ^ (String.concat "; " list) ^ "]" in
    let check (name) (actual) (expected) =
      assert_equal actual expected ?msg:(Some (sprintf "Expected %s %s but found %s" name (show_list (expected)) (show_list actual)))
    in begin
      check "result" (actual_result) (expected_result);
      check "leftovers" (actual_leftovers) (expected_leftovers)
    end
  in test_result' (process_args (fun msg -> assert_equal true false ?msg:(Some ("Unexpected error: `" ^ msg ^ "'"))) (options) [] args)

let test_error (args) expected_error () =
  let found_error = ref false in
  let check_error (error) =
    begin
      assert_equal error expected_error ?msg:(Some (sprintf "Expected error `%s' but found error `%s'" expected_error error));
      assert_equal (!found_error) false ?msg:(Some (sprintf "Observed error `%s' more than once" error));
      found_error := true;
    end
  in begin
    assert_raises ?msg:(Some "Expected exception") Arg_fail (fun () -> ignore (process_args (check_error) (options) [] args));
    assert_equal (!found_error) true ?msg:(Some (sprintf "Failed to observe expected error `%s'" expected_error))
  end

let test_help () =
  check_output (fun pr -> print_help (pr) (options))
    (""
     ^ " -c  --l-five           #5\n"
     ^ " -b  --l-four           #4\n"
     ^ "     --l-one            #1\n"
     ^ " -a  --l-three (zeta)   #3\n"
     ^ "     --l-two   (alpha)  #2\n")

let all_tests = "getopt" >:::
  [
    "result-00" >:: test_result [|""|] [] [];
    "result-01" >:: test_result [|""; "-c"|] ["5"] [];
    "result-02" >:: test_result [|""; "-b"; "-c"|] ["4";"5"] [];
    "result-03" >:: test_result [|""; "-c"; "-b"|] ["5";"4"] [];
    "result-04" >:: test_result [|""; "-cb"|] ["5";"4"] [];
    "result-05" >:: test_result [|""; "-bc"|] ["4";"5"] [];
    "result-06" >:: test_result [|""; "-bb"|] ["4";"4"] [];
    "result-07" >:: test_result [|""; "-b"; "foo"; "-c"|] ["4";"5"] ["foo"];
    "result-08" >:: test_result [|""; "quux"; "-b"; "foo"; "--l-five"; "bar"|] ["4";"5"] ["quux"; "foo"; "bar"];
    "result-09" >:: test_result [|""; "quux"; "--l-four"; "foo"; "--l-five"; "bar"|] ["4";"5"] ["quux"; "foo"; "bar"];
    "result-10" >:: test_result [|""; "quux"; "--l-four"; "foo"; "--"; "--l-five"; "bar"|] ["4"] ["quux"; "foo"; "--l-five"; "bar"];
    "result-11" >:: test_result [|""; "quux"; "--l-four"; "foo"; "--"; "-c"; "bar"|] ["4"] ["quux"; "foo"; "-c"; "bar"];
    "result-12" >:: test_result [|""; "quux"; "--l-three"; "foo"; "--"; "-c"; "bar"|] ["3:foo"] ["quux"; "-c"; "bar"];
    "result-13" >:: test_result [|""; "quux"; "--l-three=foo"; "-c"; "bar"|] ["3:foo"; "5"] ["quux"; "bar"];
    "result-14" >:: test_result [|""; "quux"; "-a=foo"; "-c"; "bar"|] ["3:foo"; "5"] ["quux"; "bar"];
    "result-15" >:: test_result [|""; "quux"; "-afoo"; "-c"; "bar"|] ["3:foo"; "5"] ["quux"; "bar"];
    "result-16" >:: test_result [|""; "quux"; "-a"; "foo"; "-c"; "bar"|] ["3:foo"; "5"] ["quux"; "bar"];
    "result-17" >:: test_result [|""; "quux"; "-a"; "-foo"; "-c"; "bar"|] ["3:-foo"; "5"] ["quux"; "bar"];
    "result-18" >:: test_result [|""; "quux"; "-ca"; "-foo"; "-c"; "bar"|] ["5"; "3:-foo"; "5"] ["quux"; "bar"];
    "result-19" >:: test_result [|""; "quux"; "--l-one"; "foo"; "--l-two"; "bar"|] ["1";"2:bar"] ["quux"; "foo"];
    "error-0" >:: test_error [|""; "-h"|] "Unknown option `-h'";
    "error-1" >:: test_error [|""; "-bh"|] "Unknown option `-h'";
    "error-2" >:: test_error [|""; "alpha"; "--foo"|] "Unknown option `--foo'";
    "error-3" >:: test_error [|""; "-c"; "-h"|] "Unknown option `-h'";
    "error-4" >:: test_error [|""; "-a"|] "Missing argument to option `-a'";
    "error-5" >:: test_error [|""; "--l-three"|] "Missing argument to option `--l-three'";
    "error-6" >:: test_error [|""; "--l-one=0"|] "Unexpected argument `0' to option `--l-one'";
    "error-7" >:: test_error [|""; "-c=0"|] "Unexpected argument `0' to option `-c'";
    "help" >:: test_help
  ]

let _ = run_test_tt_main (all_tests)

