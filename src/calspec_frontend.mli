(***************************************************************************
 This file is Copyright (C) 2011 Christoph Reichenbach

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

type day = Caltime.day
type time = Caltime.time
type interval = Caltime.interval

type partial_timespec = day option * time option * interval option

type line_nr = int

type action = ACTION_LITERAL of string
	      | ACTION_REGEX of string
	      | ACTION_LABEL of string
	      | ACTION_LIST of calspec_entry
 and calspec_entry = { declarations		: (line_nr * action * partial_timespec) list;
		       do_actions		: (line_nr * action * partial_timespec) list;
		       default_actions		: line_nr * string }

val parse_file : string -> calspec_entry
val parse_string : string -> calspec_entry
