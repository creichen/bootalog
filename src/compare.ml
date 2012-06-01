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

let join (comp1) (comp2) (x1a, x1b) (x2a, x2b) =
  let v = comp1 x1a x2a in
  if v = 0
  then comp2 x1b x2b
  else v

let collate (comp) (list1) (list2) =
  let rec c (a1) (a2) =
    match (a1, a2) with
	([], [])	-> 0
      | (_, [])	-> -1
      | ([], _)	-> 1
      | (h1::tl1,
	 h2::tl2)	-> join comp c (h1, tl1) (h2, tl2)
  in c (list1) (list2)

let array_collate (comp) (arr0) (arr1) =
  let len0 = Array.length arr0 in
  let len1 = Array.length arr1 in
  if len0 <> len1
  then len0 - len1
  else
    let rec rcmp i =
      if i >= len0
      then 0
      else
	let v0 = Array.unsafe_get arr0 i in
	let v1 = Array.unsafe_get arr1 i in
	let c = comp v0 v1 in
	if c = 0
	then rcmp (i+1)
	else c
    in rcmp 0
