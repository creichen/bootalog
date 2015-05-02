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

open Getopt
open Printf
include Frontend

let version = "0.3.0"

let boilerplate = "bootalog v" ^ version ^ "\nCopyright (C) 2012 Christoph Reichenbach\n"
  ^ "This program is Free Software under the terms of the GNU General Public Licence, v2.0 (or later)\n"

type actions =
    HELP
  | VERSION
  | INTERACTIVE

let program_files = ref []
let text_data_files = ref []

let sprintf = Printf.sprintf

let ierror (message) =
  Printf.eprintf "Interactive error: %s\n" message

let record_file reflist (c, filename) =
  begin
    reflist := filename :: !reflist;
    c
  end

let options = [
  (Some 'v', "version", NoArg (fun _ -> VERSION), "Print version information");
  (Some 'h', "help", NoArg (fun _ -> HELP), "Print this help message");
  (Some 'r', "rules", WithArg ("file", record_file program_files), "Load rules from program file");
  (Some 'd', "data", WithArg ("file", record_file text_data_files), "Load data from text database file");
]

let print_help () =
  begin
    printf "Usage: %s [options]\n" Sys.executable_name; 
    Getopt.print_help (print_string) (options);
    exit 0
  end

let print_version () =
  begin
    print_string boilerplate;
    exit 0
  end

let db = DBFrontend.create ()
let program = ProgramFrontend.create ()
let running = ref true

let rec pad n =
  if n > 0
  then begin
    print_string " ";
    pad (n-1)
  end

let read_line () =
  begin
    Printf.printf "> ";
    flush stdout;
    input_line stdin
  end

let commands_ref = ref []

let dump_table name table =
  begin
    printf "%s:\n" (Predicate.show name);
    print_string (Combined_table.show_tabular table);
    print_string "\n"
  end

let cmd_quit _ =
  running := false

let cmd_dump tablenames =
  let dump tablename =
    let name = Predicate.P tablename
    in if DB.has_table db name
      then let table = DB.get_table db name
	   in dump_table name table
      else ierror (sprintf "Table `%s' not found" tablename)
  in List.iter dump tablenames

let cmd_rules _ =
  let count = ref 0 in
  let pr_rule rule =
    begin
      Printf.printf " (* %-4d *)\t%s\n" (!count) (Rule.show rule);
      count := !count + 1
    end
  in List.iter pr_rule (ProgramFrontend.rules program)

let cmd_eval _ =
  ProgramFrontend.eval (program) (db)

let cmd_eval_and_dump _ =
  begin
    cmd_eval ();
    DB.iter db dump_table
  end

let cmd_version _ =
  print_string boilerplate

let cmd_drop tablenames =
  let drop tablename =
    let name = Predicate.P tablename
    in if DB.has_table db name
      then DB.remove_table db name
      else ierror (sprintf "Table `%s' not found" tablename)
  in List.iter drop tablenames

let print_sig (pred, signat) =
  Printf.printf "  %s/%s\n" (Predicate.show pred) (Signature.show signat)

let cmd_signatures _ = begin
    List.iter print_sig (ProgramFrontend.signatures (program));
    flush (stdout);
  end

let cmd_signature (pred_list) =
  let prpr (pred_name) =
    try
      let pred = Parser.predicate_of_name (pred_name)
      in print_sig (pred, ProgramFrontend.get_signature (program) (pred))
    with Not_found -> ierror (Printf.sprintf "Unknown predicate `%s'" pred_name)
  in begin
    List.iter prpr pred_list;
    flush (stdout);
  end

let cmd_help _ =
  let p s = begin print_string s; print_string "\n" end in
  let stringify_args (arglist) =
    String.concat " " arglist
  in
  let max_arglen = ref 0 in
  let max_cmdlen = ref 0 in
  let update_arg_stats (name, _, args, _) =
    begin
      max_cmdlen := max (!max_cmdlen) (String.length (name));
      max_arglen := max (!max_arglen) (String.length (stringify_args args))
    end in
  let pr_command (name, _, args, descr) =
    let print_padded n s =
      begin
	print_string s;
	pad (n - (String.length s))
      end
    in begin
      print_string "  > \\";
      print_padded (!max_cmdlen + 1) name;
      print_padded (!max_arglen + 1) (stringify_args args);
      Printf.printf "    (* %s *)\n" descr;
    end
  in
  let cmp_command (name, _, _, _) (name2, _, _, _) = name < name2 in
  let commands = Sort.list (cmp_command) (!commands_ref)
  in begin
    List.iter update_arg_stats (commands);
    p "Bootalog usage:";
    p "Add facts:";
    p "  > +name(\"bootalog\").                                             (* Add a single fact to the relation `name' *)";
    p "  > +tasty-food(\"chocolate\"). +tasty-food(\"ice cream\").            (* Add multiple facts *)";
    p "  > +stringify-number(1, \"one\").		                   (* Facts can be tuples. *)";
    p "Remove facts:";
    p "  > -stringify-number(1, \"one\").                                   (* You can remove undesired facts again. *)";
    p "Check facts:";
    p "  > ?(X) :- name(X).                                               (* List all matching facts. *)";
    p "";
    p "Add rules:";
    p "  > ancestor(X, Y) :- parent(X, Y).                                (* all parents Y of X are also X' ancestors. *)";
    p "  > ancestor(X, Y) :- ancestor(X, Z), ancestor(Z, Y).              (* any ancestor's ancestor Y is also X' ancestor. *)";
    p "  > joined(lhs:L, full:Z) :- v(L, R), sys-concat(L, R, concat:Z).  (* Concatenate two strings, store in labelled table. *)";
    p "  > inc-a(SUM) :- a(X), sys-add(X, 1, :SUM).                       (* increment elements of a by 1 for inc-a. *)";
    p "";
    p "Commands:";
    List.iter pr_command (commands)
  end

let commands = [
  "quit", cmd_quit, [], "Terminates the interactive session";
  "?", cmd_eval_and_dump, [], "Evaluates everything and dumps all tables";
  "eval", cmd_eval, [], "Evaluates everything";
  "dump", cmd_dump, ["table"], "Dump contents of specified table";
  "rules", cmd_rules, [], "List all current rules";
  "help", cmd_help, [], "Print command help";
  "version", cmd_version, [], "Print version and licencing information";
  "drop", cmd_drop, ["table"], "Drops a table";
  "signatures", cmd_signatures, [], "Prints the signatures of all known predicates";
  "s", cmd_signature, ["predicate"], "Prints the signature of one predicate";
]

let _ = commands_ref := commands


let from_file filename parse_function =
  try
    Parser.from_file filename parse_function
  with Sys_error msg -> begin
    ierror (sprintf "Could not open `%s': %s" filename msg);
    []
  end

(* Manifests the parsed data first.  Not suited for large fact databases. *)
(* We'll add binary database support for that later, and anything beyond that
   is out of scope for bootalog (and instead intended in scope for whatever
   datalog system we use bootalog to construct). *)
let load_text_data filename =
  List.iter (DBFrontend.add db) (from_file filename Parser.parse_text_database)

(* Manifests the parsed rules first.  Not suited for huge programs. *)
let load_rules filename =
  ProgramFrontend.import (program) (from_file filename Parser.parse_program)

let run_query (rule) =
  begin
    ProgramFrontend.eval (program) (db);
    ProgramFrontend.eval (ProgramFrontend.singleton (rule)) (db);
    dump_table (Predicate.query) (DB.get_table db Predicate.query);
    DB.remove_table db (Predicate.query)
  end

let process_command (string) =
  let commands = Str.split (Str.regexp " ") string
  in if List.length commands >= 1
    then let args = List.tl commands in
	 let cmd_name = List.hd commands in
	 let rec do_try command_list =
	   match command_list with
	       []			-> Printf.printf "Unknown command `%s'. Try \\help.\n" cmd_name
	     | (name, cmd, _, _)::tl	-> if name = cmd_name then cmd (args) else do_try tl
	 in do_try (!commands_ref)

let process_interactive (declaration) =
  match declaration with
      Program.DRule rule	-> ProgramFrontend.add (program) (rule)
    | Program.DAddFact fact	-> DBFrontend.add db fact
    | Program.DDelFact fact	-> DBFrontend.remove db fact
    | Program.DQuery rule	-> run_query (rule)

let repl () =
  let iter () =
    try
      let input = Strlib.strip_whitespace (read_line ()) in
      if String.length (input) > 0
      then match String.get input 0 with
	'\\'	-> let cmd = String.sub input 1 (String.length input - 1)
		   in process_command (cmd)
      | _	-> List.iter (process_interactive) (Parser.parse_interactive (Lexing.from_string (input)))
    with Errors.ProgramError [Errors.ParseError ((line, offset), message)] ->
      begin
	if line = 1
	then begin
	      pad (offset + 2);
	      print_string "^"
	    end;
	  Printf.eprintf "L%d %d: parse error: %s\n%!" line offset message
      end
    | Errors.ProgramError (errorlist) ->
      Printf.eprintf "%s\n%!" (Errors.show_errors errorlist)
  in while (!running) do
      iter ()
    done

let _ =
  try
    let action, _args = process_commandline (output_string stderr) options INTERACTIVE
    in begin
      List.iter load_rules !program_files;
      List.iter load_text_data !text_data_files;
      match action with
	HELP		-> print_help ()
      | VERSION		-> print_version ()
      | INTERACTIVE	-> begin
	print_string "Welcome to Bootalog!\n";
	repl ()
      end
    end
  with Arg_fail -> (prerr_string (sprintf "\nTry %s --help for usage help\n" Sys.executable_name);
		    exit 1)
  | Errors.ProgramError (errorlist) -> begin
    Printf.eprintf "%s" (Errors.show_errors errorlist);
    exit 1
  end
  | End_of_file -> exit 0
