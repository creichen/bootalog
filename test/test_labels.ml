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

let test_order (expected_values, expected_labels : string array * Label.t array) (values, labels : string array * Label.t array) () =
  let show l = "[|" ^ (String.concat ", " (Array.to_list l)) ^ "|]" in
  let showt (a, b) = "(" ^ (show a) ^ "; " ^ (show (Array.map Label.show b)) ^ ")"
  in begin
    Label.order_labels (values) (labels);
    assert_equal (expected_values, expected_labels) (values, labels) ~printer:showt ~msg:("order result")
  end

let all_tests = "labels" >:::
  [
    "label-0" >:: test_order ([||], [||]) ([||], [||]);
    "label-1" >:: test_order ([|"0"|], [|Some "a"|]) ([|"0"|], [|Some "a"|]);
    "label-2" >:: test_order ([|"0"; "1"; "2"|], [|None; None; Some "a"|]) ([|"0"; "1"; "2"|], [|None; None; Some "a"|]);
    "label-3" >:: test_order ([|"0"; "1"; "2"|], [|None; Some "a"; Some "c"|]) ([|"0"; "2"; "1"|], [|None; Some "c"; Some "a"|]);
    "label-4" >:: test_order ([|"0"; "1"; "2"; "3"|], [|Some "a"; Some "b"; Some "c"; Some "d"|])
                             ([|"2"; "0"; "3"; "1"|], [|Some "c"; Some "a"; Some "d"; Some "b"|]);
  ]


let _ = run_test_tt_main (all_tests)

