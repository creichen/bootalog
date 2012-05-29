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

let version = "0.0.1"

let boilerplate = "bootlog v" ^ version ^ "\nCopyright (C) 2012 Christoph Reichenbach\n"
  ^ "This program is Free Software under the terms of the GNU General Public Licence, v2.0 (or later)\n"

type actions = HELP
	       | VERSION

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

let _ =
  try
    let action, args = process_commandline Error.report options HELP
    in match action with
	HELP	-> print_help ()
      | VERSION	-> print_version ()
  with Arg_fail -> (prerr_string (sprintf "\nTry %s --help for usage help\n" Sys.executable_name);
		    exit 1)
