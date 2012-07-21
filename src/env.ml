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

open Hashtbl

type t = (Variable.t, Atom.t) Hashtbl.t

let fresh () = Hashtbl.create (7)

let find = Hashtbl.find

let lookup env variable =
  try Some (find env variable)
  with Not_found -> None

let bound = Hashtbl.mem

let bind = Hashtbl.replace
let unbind = Hashtbl.remove

let clear = Hashtbl.clear

let show table =
  let s var atom tail =
    ((Variable.show var) ^ ": " ^ (Atom.show atom)) :: tail
  in let body = Hashtbl.fold s table []
     in "{| " ^ (String.concat ", " body) ^ " |}"
