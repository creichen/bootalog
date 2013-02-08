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

open Lexing
open Base
open Program
open Program.Lexeme

(* Generate the ith temporary variable name for the given rule *)
let gen_temp_var_name (i) =
  Printf.sprintf "$%d" i

let default_warning_reporter = print_endline
let warning_reporter = ref (default_warning_reporter)

type pos = int * int

let predicate_of_name (name : string) =
  if Primops.name_is_primop name
  then Predicate.Primop (name, Primops.resolve (name))
  else Predicate.P (name)

(* Has to be on toplevel to be polymorphic (argh) *)
let parse_tuple_with_labels (peek, error, expect, expect_name, accept_label) (match_shortcut_variables : (string -> 'a) option) (element_descr : string) (accept_content : unit -> 'a option) : (Label.t array * 'a array) =
    begin
      expect LOparen;
      let rec parse_expecting_separator () : Label.t list * 'a list =
	match peek () with
	  LComma	-> begin
	    expect (LComma);
	    parse (false)
	  end
	| _		-> begin expect LCparen; ([], []) end
      and parse (empty_allowed : bool) : Label.t list * 'a list =
	match peek () with
	  LCparen	-> if empty_allowed then begin
                                                   expect LCparen;
	                                           ([], [])
                                                 end
                                            else error ("')' after ','")
	| LColon	-> begin
                             (* implicitly labelled parameter *)
	  		     match match_shortcut_variables with
			       None		-> error (Printf.sprintf "Implicit labelling (prefix-`:') not allowed for %ss" element_descr);
			     | Some label_to_v	-> (
	                       expect (LColon);
	                       let varname = expect_name ()
			       in let labels, values = (parse_expecting_separator ())
				  in ((Label.some (String.lowercase varname)) :: labels, (label_to_v (varname)) :: values)
			     )
	                   end
	| _		-> begin
	  let label = accept_label ()
	  in match accept_content () with
	    None	-> if empty_allowed then ([], []) else error (Printf.sprintf "Expected %s in list of %ss" element_descr element_descr)
	  | Some value	-> let labels, values = (parse_expecting_separator ())
			   in (label :: labels, value :: (values))
	end in
      let (labels, names) = parse (true)
      in begin
	(Array.of_list labels, Array.of_list names)
      end
    end


let generic_parse lexbuf =
  let past_tokens : (pos * lexeme) list ref = ref [] in
  let line = ref 1 in
  let offset = ref 0 in
  let get_pos () = (!line, !offset) in
  let warning msg =
    (!warning_reporter) (Printf.sprintf "%s: %s" (Errors.show_pos (get_pos()))  msg) in
  let error msg =
    raise (Errors.ProgramError [Errors.ParseError (get_pos(), msg)])
  in
  let error_unexpected token insth =
    error (Printf.sprintf "unexpected: %s in %s" (Program.Lexeme.show token) insth)
  in
  let set_pos (line_nr, off) =
    begin
      line := line_nr;
      offset := off;
    end in
  let lex () =
    let off = lexbuf.lex_curr_p.pos_cnum in
    let line_nr = lexbuf.lex_curr_p.pos_lnum in
    let v = Lexer.lex lexbuf
    in match v with
	LErrortoken (_, c)	-> error (Printf.sprintf "Foreign character `%c'" c)
      | _			->
	begin
	  set_pos (line_nr, off);
	  v
	end
  in

  let next () =
    match !past_tokens with
	(pos, h)::tl	-> begin past_tokens := tl; set_pos pos; h end
      | []		-> lex ()
  in
  let push_back (pos, token) =
    begin
      past_tokens := (get_pos (), token) :: !past_tokens;
      set_pos (pos)
    end
  in

  let peek () =
    let old_pos = get_pos () in
    let token = next ()
    in begin
      push_back (old_pos, token);
      token
    end
  in

  let try_next (checker) =
    let old_pos = get_pos () in
    let next_token = next ()
    in let result = checker next_token
       in begin
	 (match result with
	     None	-> push_back (old_pos, next_token)
	   | _		-> ());
	 result
       end
  in

  (* Auxiliaries for recursive descent parser *)
  let accept (entity) =
    let checker other = if other = entity then Some true else None in
    match try_next (checker) with
	Some _	-> true
      | None	-> false
  in

  let rec accept_atom () =
    let old_pos = get_pos () in
    let checker other =
      match other with
	LAtom a	-> Some a
      | LMinus  -> (let sub = accept_atom ()
		   in match sub with
		     None	-> begin push_back (old_pos, LMinus); None end
		   | Some a	-> Some (Atom.from_string ("-" ^ (Atom.to_string a))))
      | _	-> None
    in try_next checker
  in

  let temp_var_counter = ref 0 in
  let temp_body = ref [] in
  let get_wildcard () =
    let var = !temp_var_counter in
    let var_name = gen_temp_var_name (var)
    in begin
      temp_var_counter := var + 1;
      var_name
    end in

  let get_temp_var_for_assignment (atom) =
    let var_name = get_wildcard ()
    in begin
      temp_body := (Predicate.Assign atom, ([| None |], [| var_name |])) :: !temp_body;
      var_name
    end in

  let get_temp_assignments () =
    let results = List.rev (!temp_body)
    in begin
      temp_body := [];
      temp_var_counter := 0;
      results
    end in

  let check_predicate_conventions (s) =
    begin
      if String.lowercase (s) <> s
      then warning (Printf.sprintf "predicate `%s' violates naming conventions: should be lowercase" s);
      s
    end in

  let check_variable_conventions (s) =
    begin
      if String.uppercase (s) <> s
      then warning (Printf.sprintf "variable `%s' violates naming conventions: should be uppercase" s);
      s
    end in

  let accept_name_or_temp_atom () =
    let checker other =
      match other with
  	LName a		-> Some (check_variable_conventions (a))
      | LWildcard	-> Some (get_wildcard ())
      | LAtom a		-> Some (get_temp_var_for_assignment a)
      | _		-> None
    in try_next checker
  in

  let accept_name () =
    let checker other =
      match other with
  	LName a		-> Some a
      | LWildcard	-> Some (get_wildcard ())
      | _		-> None
    in try_next checker
  in

  let expect (entity) =
    if not (accept (entity))
    then error (Printf.sprintf "Expected `%s' but found `%s'" (Program.Lexeme.show entity) (Program.Lexeme.show (peek ())))
  in
(*
  let expect_atom () =
    match accept_atom () with
	None	-> error (Printf.sprintf "Expected atom (at `%s')" (Program.Lexeme.show (peek())))
      | Some a	-> a
  in
*)
  let expect_name () =
    match accept_name () with
	None	-> error (Printf.sprintf "Expected name (at `%s')" (Program.Lexeme.show (peek())))
      | Some n	-> n
  in

(*
  let expect_name_or_temp_atom () =
    match accept_name_or_temp_atom () with
	None	-> error (Printf.sprintf "Expected name or atom (at `%s')" (Program.Lexeme.show (peek())))
      | Some n	-> n
  in
*)
  let parse_list separator accept_element element_descr accept_final =
    let rec parse_expecting_separator () =
      if accept_final ()
      then []
      else if accept (separator)
      then parse_not_expecting_separator (false)
      else []
    and parse_not_expecting_separator empty_allowed =
      if empty_allowed && accept_final ()
      then []
      else match accept_element () with
	  None		-> if empty_allowed then  [] else error (Printf.sprintf "Expected %s in list of %ss" element_descr element_descr)
	| Some e	-> e :: (parse_expecting_separator ())
    in parse_not_expecting_separator (true)

  in
  let always expect () =
    Some (expect ())
  in
(*
  let perr s =
    Printf.eprintf "%s\n" s
  in
*)
  let assert_basic_predicate_in_literal (start_pos) ((head_p, _) as literal) =
    let error' e =
      begin
	set_pos (start_pos);
	error e;
      end in
    let result = match Primop_interface.primop_id (head_p) with
	None		-> literal
      | Some nid	-> error' (Errors.Parser.msg_primop_in_head (Primops.get_name nid))
    in if Predicate.is_neg (head_p)
      then error' (Errors.Parser.msg_negative_head (Literal.show literal))
      else result
  in


  let accept_label () : Label.t =
    match peek () with
      LName n	-> begin
	let old_pos = get_pos () in
	let old_token = next ()
	in match peek () with
	  LColon	-> begin expect (LColon); Label.some n end
	| LCdash	-> let new_pos = get_pos ()
			   in begin expect (LCdash); push_back (new_pos, LMinus); Label.some n end
	| _		-> begin push_back (old_pos, old_token); Label.none end
      end
    | _		-> Label.none
  in

  let parse_tuple_with_labels1 : ((string -> atom) option) -> (string) -> (unit -> atom option) -> (Label.t array * atom array) = parse_tuple_with_labels (peek, error, expect, expect_name, accept_label) in
  let parse_tuple_with_labels2 : ((string -> string) option) -> (string) -> (unit -> string option) -> (Label.t array * string array) = parse_tuple_with_labels (peek, error, expect, expect_name, accept_label) in
(*  let parse_tuple_with_labels2 : ((string -> string) option) -> (string) -> (unit -> string option) -> (Label.t array * string array) = parse_tuple_with_labelsX in*)


  (* Recursive descent parsers *)
  let rec parse_program () =
    begin
    match peek () with
	LEOF	-> []
      | _	-> let lhs = parse_rule ()
		   in lhs :: parse_program ()
    end

  and parse_interactive () =
    match peek () with
	LEOF	-> []
      | _	-> let lhs = parse_interaction ()
		   in lhs :: parse_interactive ()

  and parse_database () =
    match peek () with
	LEOF	-> []
      | _	-> let lhs = parse_fact ()
		   in lhs :: parse_database ()

  (* Auxiliary parsers *)
(*
  and parse_atom () : atom =
    if accept LMinus
    then ("-" ^ expect_atom ())
    else expect_atom ()
*)
  and parse_fact () : fact =
    let pred = expect_name ()
    in (pred, parse_tuple ())

  and parse_tuple () : tuple =
    parse_tuple_with_labels1 (None) ("atom") (accept_atom)

  and parse_base_literal_body (pred) =
    (pred, parse_tuple_with_labels2  (Some (function (s : string) -> s)) ("variable") (accept_name_or_temp_atom))

  and parse_predicate () : Predicate.t =
    match peek () with
      LEqual	-> begin ignore (accept (LEqual)); Primops.Sys.eq end
    | LName n	-> let result = predicate_of_name (expect_name())
                   in begin
		     ignore (check_predicate_conventions n);
		     result
                   end
    | _		-> error "Expected predicate name"

  and parse_base_literal () : literal =
    let neg = accept (LTilde) in
    let body = parse_base_literal_body (parse_predicate ())
    in if neg
      then Literal.neg (body)
      else body

  and parse_literal () : literal =
    parse_base_literal ()

  and parse_literals (terminator) : literal list =
    parse_list LComma (always parse_literal) "literal" (function () -> accept terminator)

  and parse_rule_tail (head) : rule = 
    match peek () with
      LPeriod	-> begin
	expect (LPeriod);
	(head, get_temp_assignments ())
      end
    | LCdash	-> begin
      expect LCdash;
      let body =  parse_literals (LPeriod) in
      let temp_body = get_temp_assignments ()
      in begin
	(head, temp_body @ body)
      end
    end
    | other -> error_unexpected other "rule"

  and parse_rule () : rule =
    let old_pos = get_pos ()
    in parse_rule_tail (assert_basic_predicate_in_literal (old_pos) (parse_base_literal ()))

  and parse_interactive_query () : rule =
    begin
      expect LQuestionmark;
      let head = parse_base_literal_body (Predicate.query)
      in parse_rule_tail (head)
    end

  and parse_interaction () =
    let dparse char action =
      begin
	expect char;
	let result = action (parse_fact ());
	in begin
	  expect LPeriod;
	  result
	end
      end
    in
    match peek () with
      LPlus		-> dparse LPlus (function a -> DAddFact a)
    | LMinus		-> dparse LMinus (function a -> DDelFact a)
    | LQuestionmark	-> DQuery (parse_interactive_query ())
    | _			-> DRule (parse_rule ())

  in (parse_program, parse_interactive, parse_database)


let parse_program lexbuf =
  let (prog, _, _) = generic_parse lexbuf
  in prog ()

let parse_interactive lexbuf =
  let (_, inter, _) = generic_parse lexbuf
  in inter ()

let parse_text_database lexbuf =
  let (_, _, db) = generic_parse lexbuf
  in db ()

(* raise Sys_error if opening failed *)
let from_file filename parse_function =
  let file = open_in filename in
  let result = parse_function (Lexing.from_channel file)
  in begin
    close_in (file);
    result
  end
