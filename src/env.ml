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
open Hashtbl


type t = (variable, atom) Hashtbl.t

let fresh () = Hashtbl.create (7)

let find = Hashtbl.find

let lookup env variable =
  try Some (find env variable)
  with Not_found -> None

let add = Hashtbl.add
(*
let bind env variable atom =
  match lookup env variable with
      None	-> (Hashtbl.add env variable atom; true)
    | Some a	-> a == atom
*)
