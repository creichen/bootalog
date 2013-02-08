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

 The author can be reached as "creichen" at the usual gmail server.

***************************************************************************)

type t = string option

let none : t = None
let some (l) : t = Some l

(* debugging only *)
let show s =
  match s with
    Some s	-> s ^ ":"
  | None	-> "#" 

let show_atom label atom =
  match label with
    None	-> (Atom.show atom)
  | Some l	-> l ^ ": " ^ (Atom.show atom)

let show_var label var =
  match label with
    None	-> var
  | Some l	-> l ^ ": " ^ (Variable.show var)

let compare lhs rhs =
  match (lhs, rhs) with
    (None, None)	-> 0
  | (None, Some _)	-> -1
  | (Some _, None)	-> 1
  | (Some l0, Some l1)	-> String.compare (l0) (l1)

let order_labels (values : 'a array) (labels : t array) =
  let values_length = Array.length values in
  let () = assert (values_length = Array.length labels) in
  let zipped = Array.init (values_length) (function i -> (Array.get values i, Array.get labels i)) in
  let compare_labels (_, i) (_, j) = compare (i) (j) in
  let export i (v, l) =
    begin
      Array.set values i v;
      Array.set labels i l;
    end
  in begin
    Array.stable_sort (compare_labels) (zipped);
    Array.iteri export (zipped);
  end

