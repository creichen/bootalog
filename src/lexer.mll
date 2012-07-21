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

{
open Program.Lexeme
open Strlib

let ret v  = v

}

let ident_tail = [ '\'' 'a'-'z' 'A'-'Z' '-' '0'-'9' ] *
let ident = [ 'a'-'z' 'A'-'Z' ] ident_tail
let posnum = ['0'-'9']+

rule lex =
    parse ","						{ ret LComma }
      | "."						{ ret LPeriod }
      | "?"						{ ret LQuestionmark }
      | ":-"						{ ret LCdash }
      | "+"						{ ret LPlus }
      | "-"						{ ret LMinus }
      | "="						{ ret LEqual }
      | "~"						{ ret LTilde }
      | "(*"						{ lex_comment 1 lexbuf }
      | "("						{ ret LOparen }
      | ")"						{ ret LCparen }
      | '_' ident_tail					{ ret LWildcard }
      | ident as i					{ ret (LName i) }
      | '\'' (ident as i)				{ ret (LAtom i) }
      | posnum as n					{ ret (LAtom n) }
      | posnum '.' posnum as n				{ ret (LAtom n) }
      | '"' (( '\\' _ | [^ '\\' '"'])* as qs) '"'	{ ret (LAtom (Strlib.dequote qs)) }
      | [ ' ' '\t' '\r' '\n' ]+				{ lex lexbuf }
      | eof						{ ret (LEOF) }
      | _ as s						{ ret (LErrortoken ((0, 0), s)) }
and lex_comment depth =
  parse "*)"		{ if depth = 1 then lex lexbuf else lex_comment (depth - 1) lexbuf }
      | "(*"		{ lex_comment (depth + 1) lexbuf }
      | _		{ lex_comment depth lexbuf }

{}
