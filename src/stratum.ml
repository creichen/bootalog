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

module PredicateSet = Predicate_set
module BaseRule = Base_rule

type t =
  { pss		: PredicateSet.t;
    base	: Rule.t list;
    delta	: Rule.t list }

    let show_n label stratum =
      let show_rules rules =
	String.concat "" (List.map (function rule -> "  " ^ BaseRule.show rule ^ "\n") rules)
      in ("== " ^ label ^ "<" ^ (PredicateSet.show stratum.pss) ^ ">}\n"
	  ^ "- base:\n"
	  ^ (show_rules stratum.base)
	  ^ "- delta:\n"
	  ^ (show_rules stratum.delta))

    let show = show_n ""

    let normalise stratum =
      { pss	= stratum.pss;
	base	= List.sort BaseRule.compare stratum.base;
	delta	= List.sort BaseRule.compare stratum.delta; }

    let equal stratum0 stratum1 =
      let s0 = normalise stratum0 in
      let s1 = normalise stratum1
      in (PredicateSet.equal s0.pss s1.pss
	  && s0.base = s1.base
	    && s0.delta = s1.delta)
