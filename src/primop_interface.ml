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

(* Primop interface.  Does not contain actual primops; cf primops_library.ml *)

type id = Predicate.primop_id

type variable_mode = Bound | Free | Any

let show_variable_mode vm =
  match vm with
    Bound	-> "b"
  | Free	-> "f"
  | Any		-> "_"

type access_cost = (* use simplified access cost model for now *)
  { cost_simple : int }

let show_cost (cost) =
  Printf.sprintf "cost:%d" cost.cost_simple

type access_mode =
  { variable_modes   : variable_mode list;
    evaluator	     : Predicate.evaluator;
    cost             : access_cost }

let show_access_mode (amode) =
  Printf.sprintf "{[%s]; %s}" (String.concat "" (List.map show_variable_mode (amode.variable_modes))) (show_cost (amode.cost))

let cost (simple) =
  { cost_simple = simple }

let total_cost (cost) =
  cost.cost_simple

let zero_cost = cost (0)
let combine_cost (c0) (c1) = cost (c0.cost_simple + c1.cost_simple)

let compare_cost ca cb = ca.cost_simple - cb.cost_simple

let show_cost (cost) = Printf.sprintf "%d" cost.cost_simple

let access_modes (var_modes : variable_mode list) (access_modes : access_mode list) : access_mode list =
  let rec is_match sides =
    match sides with
      [], []			-> true
    | Any::tl, _::tl'		-> is_match (tl, tl')
    | _::tl, Any::tl'		-> is_match (tl, tl')
    | Bound::tl, Bound::tl'	-> is_match (tl, tl')
    | Free::tl, Free::tl'	-> is_match (tl, tl')
    | _				-> false
  in let rec find_all choices =
       match choices with
	 []		-> []
       | h::tl		-> let tl' = find_all tl
			   in if is_match (h.variable_modes, var_modes) then h::tl' else tl'
  in find_all access_modes

let primops_nr = ref 0

let primops_list = ref []

let primops_names = ref []

let primops_table : (string, id) Hashtbl.t = Hashtbl.create 47

let primops_signatures : (Predicate.t, Signature.t) Hashtbl.t = Hashtbl.create 47

let register (name : string) (numargs : int) (named_args : string list) (modes : access_mode list) =
  let nr = !primops_nr
  in begin
    primops_nr := nr + 1;
    primops_list := modes :: !primops_list;
    primops_names := name :: !primops_names;
    Hashtbl.replace primops_table name nr;
    let predicate = Predicate.Primop (name, nr)
    in begin
      Hashtbl.replace primops_signatures (predicate) (Signature.make (numargs) (named_args));
      predicate
    end
  end

let primop_id (predicate) =
  match predicate with
    Predicate.Primop (_, nr)	-> Some nr
  | Predicate.Linked (_, nr, _)	-> Some nr
  | _				-> None
