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
open Eval
open Getopt
open Printf

let version = "0.0.2"

let boilerplate = "bootalog v" ^ version ^ "\nCopyright (C) 2012 Christoph Reichenbach\n"
  ^ "This program is Free Software under the terms of the GNU General Public Licence, v2.0 (or later)\n"

type actions =
    HELP
  | VERSION
  | INTERACTIVE

let options = [
  (Some 'v', "version", NoArg (fun _ -> VERSION), "Print version information");
  (Some 'h', "help", NoArg (fun _ -> HELP), "Print this help message")
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

let db = Frontend.DBFrontend.create ()
let ruleset = ref []
let running = ref true

let read_line () =
  begin
    Printf.printf "> ";
    flush stdout;
    input_line stdin
  end

let commands_ref = ref []

let cmd_quit _ =
  running := false

let commands = [
  "quit", cmd_quit, "Terminates the interactive session"
]

let _ = commands_ref := commands

let process_command (string) =
  let commands = Str.split (Str.regexp " ") string
  in if List.length commands >= 1
    then let args = List.tl commands in
	 let cmd_name = List.hd commands in
	 let rec do_try command_list =
	   match command_list with
	       []			-> Printf.printf "Unknown command `%s'. Try \\help.\n" cmd_name
	     | (name, cmd, _)::tl	-> if name = cmd_name then cmd (args) else do_try tl
	 in do_try (!commands_ref)

let process_interactive (declaration) =
  match declaration with
      Program.DRule rule	-> ruleset := rule :: !ruleset
    | Program.DAddFact fact	-> Frontend.DBFrontend.add db fact
    | Program.DDelFact fact	-> Frontend.DBFrontend.remove db fact

let repl () =
  let iter () =
    try
      let input = Strlib.strip_whitespace (read_line ()) in
      if String.length (input) > 0
      then match String.get input 0 with
	'\\'	-> let cmd = String.sub input 1 (String.length input - 1)
		   in process_command (cmd)
      | _	-> List.iter (process_interactive) (Parser.parse_interactive (Lexing.from_string (input)))
    with Parser.ParseError (line, offset, message) ->
      begin
	if line = 1
	then Printf.printf " %*c^\n" offset ' ';
	Printf.printf "L%d %d: parse error: %s\n" line offset message
      end
  in while (!running) do
      iter ()
    done

let _ =
  try
    let action, args = process_commandline Error.report options INTERACTIVE
    in match action with
      HELP		-> print_help ()
    | VERSION		-> print_version ()
    | INTERACTIVE	-> begin
      print_string "Welcome to Bootalog!\n";
      repl ()
    end
  with Arg_fail -> (prerr_string (sprintf "\nTry %s --help for usage help\n" Sys.executable_name);
		    exit 1)
