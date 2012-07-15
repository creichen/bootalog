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

let lex s =
  let buf = Lexing.from_string s in
  let rec lx () =
    match Lexer.lex buf with
	(_, LEOF)		-> []
      | (l, LErrortoken (d, c))	-> raise (Failure (Printf.sprintf "failed to parse `%s': L%d:%d (%c)" (String.escaped s) l d c))
      | (_, other)		-> other :: (lx())
  in lx()


let check_lex expected string () =
  assert_equal expected (lex string)

let check_parse_p expected string () =
  let result = (Parser.parse_program (Lexing.from_string string)) in
  let show s = String.concat "; " (List.map Rule.show s)
  in assert_equal expected result ?msg:(Some (Printf.sprintf "Parse mismatch:\nactual  :%s\nexpected: %s\n" (show result) (show expected)))

let check_parse_i expected string () =
  assert_equal expected (Parser.parse_interactive (Lexing.from_string string))

let check_parse_d expected string () =
  assert_equal expected (Parser.parse_text_database (Lexing.from_string string))

let check_eq msg expected actual =
  assert_equal expected actual ?msg:(Some msg)


let p(x, y) =
  (Predicate.P "p", [|x; y|])

let query(x, y) =
  (Predicate.query, [|x; y|])

let q(x) =
  (Predicate.P "q", [|x|])

let q2(x, y) =
  (Predicate.P "q", [|x; y|])

let atom(x) =
  (Predicate.P "atom", [|x|])

let tmpvar(x) =
  Parser.gen_temp_var_name (x)

let r() =
  (Predicate.P "r", [||])

let assign(var, value) =
  (Predicate.Assign value, [|tmpvar (var)|])

let all_tests = "frontend" >:::
  [
    "comma" >:: check_lex [LComma] ",";
    "plus" >:: check_lex [LQuestionmark; LCdash; LPlus] " ? :- + ";
    "wildcard" >:: check_lex [LMinus; LPeriod; LWildcard] "- . _";
    "parenname" >:: check_lex [LOparen; LName "foo-bar"; LCparen] "(foo-bar)";
    "arith" >:: check_lex [LAtom "0"; LPlus; LAtom "1"] "0+1";
    "narith" >:: check_lex [LAtom "0"; LMinus; LAtom "1"] "0-1";
    "atom" >:: check_lex [LAtom "foo"] " 'foo ";
    "string0" >:: check_lex [LAtom "foo"] " \"foo\" ";
    "string1" >:: check_lex [LAtom "fo\"o"] " \"fo\\\"o\" ";
    "string2" >:: check_lex [LAtom "f\\o\"o"] " \"f\\\\o\\\"o\" ";
    "parse-p-0" >:: check_parse_p [(r(), [])] "r().";
    "parse-p-1" >:: check_parse_p [(q("X"), [])] "q(X) :- .";
    "parse-p-2" >:: check_parse_p [(p("X", "Y"), [q("X"); q("Y")])] "p(X,Y) :- q(X), q(Y).";
    "parse-p-3" >:: check_parse_p [(q("X"), [q("X")]); (r(), [])] "q(X) :- q(X). r().";
    "parse-p-4" >:: check_parse_p [(q("X"), [q("X")])] "q(X) :- q(X).";
    "parse-p-lit-0" >:: check_parse_p [(q("X"), [assign(0,"42"); p("X", tmpvar(0))])] "q(X) :- p(X, 42).";
    "parse-p-lit-1" >:: check_parse_p [(q("X"), [assign(0,"23"); assign(1, "42"); p(tmpvar(0), tmpvar(1))])] "q(X) :- p(23, \"42\").";
    "parse-p-lit-2" >:: check_parse_p [(q(tmpvar(0)), [assign(0,"42")])] "q(42).";
    "parse-p-lit-3" >:: check_parse_p [(q(tmpvar(0)), [assign(0,"teatime")])] "q('teatime).";
    "parse-p-builtin-0" >:: check_parse_p [(q("X"), [assign(0,"foobar"); (Primops.Sys.concat, [|"X"; "Y"; tmpvar(0)|])])] "q(X) :- sys-concat(X,Y,\"foobar\").";
    "parse-i-0" >:: check_parse_i [Program.DAddFact ("q", [|"1"|])] "+q(1).";
    "parse-i-1" >:: check_parse_i [Program.DDelFact ("q", [|"1"|])] "-q(1).";
    "parse-i-2" >:: check_parse_i [Program.DRule (p("X", "Y"), [q("X"); q("Y")])] "p(X,Y) :- q(X), q(Y).";
    "parse-i-3" >:: check_parse_i [Program.DRule (p("X", "Y"), [q2("X", "Y"); q2("Y", "X")])] "p(X,Y) :- q(X, Y), q( Y, X  ).";
    "parse-i-4" >:: check_parse_i [Program.DAddFact ("q", [|"1"|]); Program.DDelFact ("q", [|"2"|])] "+q(1). -q(2).";
    "parse-i-5" >:: check_parse_i [Program.DQuery (query("X", "Y"), [q2("X", "Y"); q2("Y", "X")])] "?(X,Y) :- q(X, Y), q( Y, X  ).";
    "parse-comment-0" >:: check_parse_i [Program.DDelFact ("q", [|"1"|])] "-q(1 (* comment in the middle *)).";
    "parse-comment-1" >:: check_parse_i [Program.DDelFact ("q", [|"1"|])] "-q(1) (* comment (* nested *) in the middle *).";
    "parse-db-0" >:: check_parse_d [("q", [|"1"|]); ("q", [|"2"|])] "q(1) q(2)";
  ]

let _ = run_test_tt_main (all_tests)
