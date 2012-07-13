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

type base_t = string
type primop_id = int

type evaluator = Variable.t array -> Env.t -> (Env.t -> unit) -> unit

type t =
  P		of base_t
| Delta		of base_t
| Primop        of string * primop_id
(*
| Linked	of primop_id * string * evaluator
*)

let atom = P "atom"
let query = P "?"  (* used for interactive queries *)

let is_delta s =
  match s with
    Delta _	-> true
  | _		-> false

let show (p) =
  match p with
    P p			-> p
  | Delta d		-> "D[" ^ d ^ "]"
  | Primop (s, _)	-> s

let delta s =
  match s with
    P p			-> Delta p
  | Delta d		-> raise (Failure ("Attempted deltafication of delta `"^d^"'"))
  | Primop (s, _)	-> raise (Failure ("Attempted deltafication of primop `"^s^"'"))

let compare l r =
  match (l, r) with
  | ((P a,P b)
	| (Delta a, Delta b))		-> String.compare a b
  | (Primop (_, a), Primop (_, b))	-> b - a
  | (P _, _)				-> -1
  | (Delta _, P _)			-> 1
  | (Delta _, _)			-> -1
  | (Primop _, _)			-> 1
