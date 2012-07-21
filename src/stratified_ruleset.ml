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

type t = Stratum.t list

let show strata =
  let rec show_i i args =
    match args with
      []		-> ""
    | h::tl	-> (Stratum.show_n (Printf.sprintf "%i" i) h) ^ "\n" ^ (show_i (i+1) tl)
  in show_i 0 strata

let normalise =
  List.map Stratum.normalise

let equal =
  let rec is_eq r0 r1 =
    match (r0, r1) with
      ([], [])	-> true
    | (h0::tl0,
       h1::tl1)	-> if Stratum.equal h0 h1 then is_eq tl0 tl1 else false
    | _		-> false
  in is_eq
