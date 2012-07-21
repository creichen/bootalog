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

open Base

module Lexeme =
  struct
    type lexeme =
	LAtom of string
      | LName of string
      | LWildcard
      | LComma
      | LQuestionmark
      | LPeriod
      | LCdash
      | LPlus
      | LMinus
      | LEqual
      | LTilde
      | LOparen
      | LCparen
      | LEOF
      | LErrortoken of (int * int) * char

    let show l =
      match l with
	  LAtom s	-> "\"" ^ (String.escaped s) ^ "\""
	| LName s	-> s
	| LWildcard	-> "-"
	| LComma	-> ","
	| LQuestionmark	-> "?"
	| LPeriod	-> "."
	| LCdash	-> ":-"
	| LPlus		-> "+"
	| LMinus	-> "-"
	| LEqual	-> "="
	| LTilde	-> "~"
	| LOparen	-> "("
	| LCparen	-> ")"
	| LEOF		-> "end-of-file"
	| LErrortoken _	-> "*error*"
  end

type interactive_input =
    DRule of rule
  | DAddFact of fact
  | DDelFact of fact
  | DQuery of rule  (* uses Predicate.query *)

