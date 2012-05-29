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


type t = (atom list, unit) Hashtbl.t
let create () = Hashtbl.create (17)

let contains table tuple = Hashtbl.mem table tuple

let insert table tuple = Hashtbl.add table tuple ()

let bind_all table variables env (continuation) =
    (* fixme: dosn't do unbinding *)
    (* fixme: dosn't handle P(X, X) *)
  let bindings = List.map (Env.lookup env) variables in
  let rec bind args =
    match args with
	([], [], []) -> true
      | (var::vartail, None::btail, atom::vtail) -> Env.add env var atom; bind (vartail, btail, vtail)
      | (var::vartail, Some a::btail, atom::vtail) -> if a = atom then bind (vartail, btail, vtail) else false
      | (_, _, _) -> raise Malformed 
  in
  let process tuple _ =
    if bind (variables, bindings, tuple)
    then continuation env
  in Hashtbl.iter process table
