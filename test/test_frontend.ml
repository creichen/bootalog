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
open Frontend
open Program.Lexeme
open OUnit
open Error_test_helper

let lex s =
  let buf = Lexing.from_string s in
  let rec lx () =
    match Lexer.lex buf with
	(LEOF)			-> []
      | (LErrortoken (pos, c))	-> raise (Failure (Printf.sprintf "failed to tokenise `%s': %s (%c)" (String.escaped s) (Errors.show_pos pos) c))
      | (other)			-> other :: (lx())
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
  (Predicate.P "p", Tuple.positional [|x; y|])

let query(x, y) =
  (Predicate.query, Tuple.positional [|x; y|])

let q(x) =
  (Predicate.P "q", Tuple.positional [|x|])

let q2(x, y) =
  (Predicate.P "q", Tuple.positional [|x; y|])

let atom(x) =
  (Predicate.P "atom", Tuple.positional [|x|])

let tmpvar(x) =
  Parser.gen_temp_var_name (x)

let r() =
  (Predicate.P "r", Tuple.positional [||])

let assign(var, value) =
  (Predicate.Assign value, Tuple.positional [|tmpvar (var)|])

let check_with_warnings (expected_warnings) (check) () =
  let warnings_list = ref [] in
  let warn msg = warnings_list := msg :: !warnings_list in
  begin
    Parser.warning_reporter := warn;
    check ();
    assert_equal (expected_warnings) (List.rev (!warnings_list))
      ~msg:"frontend warnings"
      ~printer:(function sl -> String.concat ";" (List.map (function s -> "`" ^ s ^ "'") sl));
    Parser.warning_reporter := Parser.default_warning_reporter;
  end

let some = Label.some
let none = Label.none

module Semantic =
  (* Semantic analysis checks *)
  struct
    module PFrontend = Frontend.ProgramFrontend
    (* labelled *)
    let p(a, b) : Literal.t = (Predicate.P "p", ([|some "a"; some "b"|], [|a; b|]))
    let sig_p = Signature.make 0 ["a"; "b"]
    let q(bar, foo, quux) : Literal.t = (Predicate.P "q", ([|some "bar"; some "foo"; some "quux"|], [|bar; foo; quux|]))
    (* partially labelled or unlabelled *)
    let r(x0, x1) : Literal.t = (Predicate.P "r", ([|none; none|], [|x0; x1|]))
    let s(x0, a, b) : Literal.t = (Predicate.P "s", ([|none; some "a"; some "b"|], [|x0; a; b|]))
    let sig_s = Signature.make 1 ["a"; "b"]

    let x = "X"
    let y = "Y"
    let z = "Z"

    let check (expected_rules) (string) () =
      let program = PFrontend.create() in
      let parsed_rules = Parser.parse_program (Lexing.from_string string) in
      let show s = String.concat "; " (List.map Rule.show s)
      in begin
	PFrontend.import (program) (parsed_rules);
	assert_equal (expected_rules) (PFrontend.rules program) ~printer:show;
      end

    let base = check [(p(y, x), [r(x, y)])] "p(a:Y, b:X) :- r(X, Y)."
    let reorder0 = check [(p(x, y), [r(x, y)])] "p(b:Y, a:X) :- r(X, Y)."
    let reorder1 = check [(r(x, y), [q(x, x, y)])] "r(X, Y) :- q(foo:X, quux:Y, bar:X)."
    let reorder2 = check [(s(x, x, y), [r(x, y)])] "s(X, b:Y, a:X) :- r(X, Y)"

    let err0 = expect_errors [Errors.DuplicateLabel(Predicate.P "s", "foo", ((Predicate.P "s", ([|some "foo"; some "foo"|], [|x; y|])), [r(x, y)]))]
               (check [] "s(foo:X, foo:Y) :- r(X, Y).")

    let err1 = expect_errors [Errors.PositionalAfterNominal(Predicate.P "s", "foo", ((Predicate.P "s", ([|some "foo"; none|], [|x; y|])), [r(x, y)]))]
               (check [] "s(foo:X, Y) :- r(X, Y).")

    let err2 = expect_errors [Errors.SignatureMismatch(Predicate.P "p", sig_p, Signature.make 0 ["a"; "c"], ((Predicate.P "p", ([|some "a"; some "c"|], [|y; x|])), [r(x, y)]))]
               (check [] "p(a:Y, b:X) :- r(X, Y).  p(a:Y, c:X) :- r(X, Y).")

    let err3 = expect_errors [Errors.SignatureMismatch(Predicate.P "p", sig_p, Signature.make 0 ["a"; "b"; "c"], ((Predicate.P "p", ([|some "a"; some "b"; some "c"|], [|y; x; x|])), [r(x, y)]))]
               (check [] "p(a:Y, b:X) :- r(X, Y).  p(a:Y, b:X, c:X) :- r(X, Y).")

    let err4 = expect_errors [Errors.SignatureMismatch(Predicate.P "p", sig_p, Signature.make 0 ["a"], ((Predicate.P "p", ([|some "a"|], [|y|])), [r(x, y)]))]
               (check [] "p(a:Y, b:X) :- r(X, Y).  p(a:Y) :- r(X, Y).")

    let err5 = expect_errors [Errors.SignatureMismatch(Predicate.P "p", sig_p, Signature.make 1 ["a"], ((Predicate.P "p", ([|none; some "a"|], [|x; y|])), [r(x, y)]))]
               (check [] "p(a:Y, b:X) :- r(X, Y).  p(X, a:Y) :- r(X, Y).")

    let err6 = expect_errors [Errors.SignatureMismatch(Predicate.P "s", sig_s, Signature.make 0 ["a"; "b"], (r(x, x), [(Predicate.P "s", ([|some "a"; some "b"|], [|x; y|]))]))]
               (check [] "s(X, b:Y, a:X) :- r(X, Y).  r(X, X) :- s(b:Y, a:X).")

    let err7 = expect_errors [Errors.SignatureMismatch(Predicate.P "s", sig_s, Signature.make 2 ["a"; "b"], (r(x, x), [(Predicate.P "s", ([|none; none; some "a"; some "b"|], [|x; y; x; y|]))]))]
               (check [] "s(X, b:Y, a:X) :- r(X, Y).  r(X, X) :- s(X, Y, b:Y, a:X).")

    let err8 = expect_errors [Errors.SignatureMismatch(Predicate.P "s", sig_s, Signature.make 2 ["b"], (r(x, x), [(Predicate.P "s", ([|none; none; some "b"|], [|x; y; y|]))]))]
               (check [] "s(X, b:Y, a:X) :- r(X, Y).  r(X, X) :- s(X, Y, b:Y).")
  end

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
    "parse-warn-pred-0" >:: check_with_warnings
                             ["L1 7: predicate `BADNAME' violates naming conventions: should be lowercase"]
                             (check_parse_p [(q("X"), [(Predicate.P "BADNAME", Tuple.positional [|"X"|])])] "q(X) :- BADNAME(X).");
    "parse-warn-pred-1" >:: check_with_warnings
                             ["L1 0: predicate `BADNAME' violates naming conventions: should be lowercase"]
                             (check_parse_p [((Predicate.P "BADNAME", Tuple.positional [|"X"|]), [])] "BADNAME(X).");
    "parse-warn-var-0" >:: check_with_warnings
                             ["L1 10: variable `z' violates naming conventions: should be uppercase"]
                             (check_parse_p [(q("X"), [(Predicate.P "n", Tuple.positional [|"z"|])])] "q(X) :- n(z).");
    "parse-warn-var-1" >:: check_with_warnings
                             ["L1 2: variable `z' violates naming conventions: should be uppercase"]
                             (check_parse_p [(q("z"), [(Predicate.P "n", Tuple.positional [|"X"|])])] "q(z) :- n(X).");
    "parse-neg" >:: check_parse_p [(q("X"), [Literal.neg (q("X"))])] "q(X) :- ~q(X).";
    "parse-neg-fail-head" >:: expect_errors [Errors.ParseError ((1, 0), Errors.Parser.msg_negative_head "~q(X)")]
                              (check_parse_p [(Literal.neg (q("X")), [(q("X"))])] "~q(X) :- q(X).");
    "parse-labelled-0" >:: check_parse_p [((Predicate.P "foo", ([|some "b"|], [|"X"|])), [atom("X")])] "foo(b:X) :- atom(X).";
    "parse-labelled-1" >:: check_parse_p [((Predicate.P "foo", ([|some "b"; some  "a"|], [|"X"; "Y"|])), [atom("X"); atom("Y")])] "foo(b:X, a:Y) :- atom(X), atom(Y).";
    "parse-labelled-2" >:: check_parse_p [((Predicate.P "foo", ([|some "b"; none|], [|"X"; "Y"|])), [atom("X"); atom("Y")])] "foo(b:X, Y) :- atom(X), atom(Y).";  (* disallowed by semantic analysis, but should pass the parser *)
    "parse-labelled-3" >:: check_parse_p [((Predicate.P "foo", ([|none; some "b"|], [|"X"; "Y"|])), [atom("X"); atom("Y")])] "foo(X, b:Y) :- atom(X), atom(Y).";
    "parse-implicitly-labelled-0" >:: (check_parse_p [((Predicate.P "foo", ([|some "x"; some "foo"|], [|"X"; "FOO"|])), [atom("X")])] "foo(:X,:FOO) :- atom(X).");
    "parse-p-lit-0" >:: check_parse_p [(q("X"), [assign(0,"42"); p("X", tmpvar(0))])] "q(X) :- p(X, 42).";
    "parse-p-lit-1" >:: check_parse_p [(q("X"), [assign(0,"23"); assign(1, "42"); p(tmpvar(0), tmpvar(1))])] "q(X) :- p(23, \"42\").";
    "parse-p-lit-2" >:: check_parse_p [(q(tmpvar(0)), [assign(0,"42")])] "q(42).";
    "parse-p-lit-3" >:: check_parse_p [(q(tmpvar(0)), [assign(0,"teatime")])] "q('teatime).";
    "parse-p-builtin-0" >:: check_parse_p [(q("X"), [assign(0,"foobar"); (Primops.Sys.concat, Tuple.positional [|"X"; "Y"; tmpvar(0)|])])] "q(X) :- sys-concat(X,Y,\"foobar\").";
    "parse-builtin-fail-head" >:: expect_errors [Errors.ParseError ((1, 0), Errors.Parser.msg_primop_in_head "sys-length")]
                                  (check_parse_p [((Primops.Sys.length, Tuple.positional [|"X"|]), [(q("X"))])] "sys-length(X) :- q(X).");
    "parse-p-eq-0" >:: check_parse_p [(q("X"), [assign(0,"foobar"); (Primops.Sys.eq, Tuple.positional [|"X"; tmpvar(0)|])])] "q(X) :- =(X,\"foobar\").";
    "parse-i-0" >:: check_parse_i [Program.DAddFact ("q", Tuple.positional [|"1"|])] "+q(1).";
    "parse-i-1" >:: check_parse_i [Program.DDelFact ("q", Tuple.positional [|"1"|])] "-q(1).";
    "parse-i-2" >:: check_parse_i [Program.DRule (p("X", "Y"), [q("X"); q("Y")])] "p(X,Y) :- q(X), q(Y).";
    "parse-i-3" >:: check_parse_i [Program.DRule (p("X", "Y"), [q2("X", "Y"); q2("Y", "X")])] "p(X,Y) :- q(X, Y), q( Y, X  ).";
    "parse-i-4" >:: check_parse_i [Program.DAddFact ("q", Tuple.positional [|"1"|]); Program.DDelFact ("q", Tuple.positional [|"2"|])] "+q(1). -q(2).";
    "parse-i-5" >:: check_parse_i [Program.DQuery (query("X", "Y"), [q2("X", "Y"); q2("Y", "X")])] "?(X,Y) :- q(X, Y), q( Y, X  ).";
    "parse-comment-0" >:: check_parse_i [Program.DDelFact ("q", Tuple.positional [|"1"|])] "-q(1 (* comment in the middle *)).";
    "parse-comment-1" >:: check_parse_i [Program.DDelFact ("q", Tuple.positional [|"1"|])] "-q(1) (* comment (* nested *) in the middle *).";
    "parse-db-0" >:: check_parse_d [("q", Tuple.positional [|"1"|]); ("q", Tuple.positional [|"2"|])] "q(1) q(2)";
    "parse-db-1" >:: check_parse_d [("q", Tuple.positional [|"-1"|])] "q(-1)";
    "semantic-base" >:: Semantic.base;
    "semantic-reorder0" >:: Semantic.reorder0;
    "semantic-reorder1" >:: Semantic.reorder1;
    "semantic-reorder2" >:: Semantic.reorder2;
    "semantic-err0" >:: Semantic.err0;
    "semantic-err1" >:: Semantic.err1;
    "semantic-err2" >:: Semantic.err2;
    "semantic-err3" >:: Semantic.err3;
    "semantic-err4" >:: Semantic.err4;
    "semantic-err5" >:: Semantic.err5;
    "semantic-err6" >:: Semantic.err6;
    "semantic-err7" >:: Semantic.err7;
    "semantic-err8" >:: Semantic.err8;
  ]

let _ = run_test_tt_main (all_tests)
