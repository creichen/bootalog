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

type t = Atom.t array
let show elts = "(" ^ (String.concat ", " (List.map Atom.show (Array.to_list elts))) ^ ")"
let sort = List.sort (Compare.array_collate (String.compare))

let string_sizes (tuple) =
  List.map (function t -> (String.length (Atom.show t))) (Array.to_list tuple)

let show_padded sizes tuple =
  let rec s a b =
    match (a, b) with
      (size::sl, atom::al)	-> (Printf.sprintf "%-*s" size (Atom.show atom)) :: (s sl al)
    | (_, [])			-> []
    | ([], atom::al)		-> (Atom.show atom) :: (s [] al)
  in String.concat "" (s sizes (Array.to_list tuple))

let merge_string_sizes sizes0 sizes1 =
  let rec m a b =
    match (a, b) with
      ([], tl)	-> tl
    | (tl, [])	-> tl
    | (h1::tl1,
       h2::tl2)	-> (if h1 > h2 then h1 else h2)::(m tl1 tl2)
  in m sizes0 sizes1
