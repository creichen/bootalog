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

let str c = Printf.sprintf "%c" c

let dequote string =
  let wrpos = ref 0 in
  let unskip = ref false in
  let append c = begin
    String.set string (!wrpos) c;
    wrpos := 1 + (!wrpos)
  end
  in begin
    for rdpos = 0 to (String.length string) - 1 do
      let c = String.get string rdpos
      in if !unskip
	then begin
	  append c;
	  unskip := false
	end
	else if c = '\\'
	then unskip := true
	else append c
    done;
    String.sub string 1 ((!wrpos) - 2)
  end

let is_blank c = match c with
    ' '		-> true
  | '\t'	-> true
  | '\n'	-> true
  | '\r'	-> true
  | _		-> false

let strip_whitespace (string) =
  let max = String.length (string) - 1 in
  let l = ref 0 in
  let r = ref max in
  (while !l < max && is_blank(string.[!l]) do l := 1 + !l done;
   if !l = max
   then ""
   else begin
     while is_blank(string.[!r]) do r := !r -1 done;
     String.sub string !l (!r - !l + 1)
   end
  )
