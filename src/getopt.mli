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

(** Module for getopt_long-style command-line processing *)

(** Individual argument processors *)
type 'a opt_proc = NoArg	of ('a -> 'a) (** option expects no argument *)
	         | WithArg	of string * ('a * string -> 'a) (** option expects argument *)

(** Individual argument spec.  The final string here is a brief description of the parameter. *)
type 'a opt = char option * string * 'a opt_proc * string

val print_help : (string -> unit) -> 'a opt list -> unit

exception Arg_fail

(** Process all command line arguments according to an argument spec.
    Parameters are the error handler, argument spec, and default return value. *)
val process_commandline : (string -> unit) -> 'a opt list -> 'a -> 'a * string list


(** As process_commandline, but processes a string array with arguments (arguments starting at 1). *)
val process_args : (string -> unit) -> 'a opt list -> 'a -> string array -> 'a * string list

