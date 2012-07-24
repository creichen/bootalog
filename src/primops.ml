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

open Primop_interface
(* Explicitly include any primop sub-libraries here! *)

type primop_id = int

type primop = string * (access_mode list)

type adapter_interface =
  { get	 : int -> Base.atom;
    set  : int -> Base.atom -> unit;
    cont : unit -> unit }

type fast_adapter_interface =
  { get'	 : int -> Base.atom;
    set'	 : int -> Base.atom -> unit;
    clear'	 : int -> unit;
    cont'	 : unit -> unit }

let bind = Env.bind
let find = Env.find

let adapter (m : adapter_interface -> unit) vars env mcont =
  let setvars = ref [] in
  let get i =
    begin
(*
      Printf.eprintf " Getting vars at %d\n%!" i;
      Printf.eprintf "  -> `%s'\n%!" (Array.get vars i);
      Printf.eprintf "     (in env=%s)\n%!" (Env.show env);
*)
      Env.find env (Array.get vars i)
    end in
  let set i value =
    let var = Array.get vars i
    in begin
      Env.bind env var value;
      setvars := var :: !setvars
    end
  in
  let cont () = begin
    mcont env;
    List.iter (Env.unbind env) (!setvars);
    setvars := []
  end
  in m { get = get; set = set; cont = cont }

let fast_adapter (m : fast_adapter_interface -> unit) vars env mcont =
  let get i =
    begin
(*
      Printf.eprintf " Getting vars at %d\n%!" i;
      Printf.eprintf "  -> `%s'\n%!" (Array.get vars i);
      Printf.eprintf "     (in env=%s)\n%!" (Env.show env);
*)
      Env.find env (Array.get vars i)
    end in
  let set i value =
    let var = Array.get vars i
    in begin
      Env.bind env var value
    end
  in
  let clear i =
    Env.unbind env (Array.get vars i)
  in
  let cont () = begin
    mcont env
  end
  in m { get' = get; set' = set; cont' = cont; clear' = clear }


let mode m c e = {
  variable_modes	= m;
  evaluator		= adapter e;
  cost			= c
}

let fmode m c e = {
  variable_modes	= m;
  evaluator		= fast_adapter e;
  cost			= c
}

let min_cost = { cost_simple = 1 }
let write_cost n = { cost_simple = 3 * n }

let bb = Bound
let ff = Free

module Sys =
  struct
    let eq = register "=" 2 [] [
      fmode [bb; ff] (write_cost 1) (* *) (function { get'; set'; cont'; clear' } -> begin set' 1 (get' 0); cont' (); clear' 1 end);
      fmode [ff; bb] (write_cost 1) (* *) (function { get'; set'; cont'; clear' } -> begin set' 0 (get' 1); cont' (); clear' 0 end);
      fmode [bb; bb] min_cost	    (* *) (function { get'; set'=_; cont'; clear'=_ } -> begin if get' 0 = get' 1 then cont' () end)
    ]

    let concat = register "sys-concat" 2 ["concat"] [
      fmode [bb; bb; bb] min_cost (* *) (function { get'; set'=_; cont'; clear'=_ } -> begin if get'(0) ^ get'(1) = get'(2) then cont' () end);
      fmode [bb; bb; ff] (write_cost 3) (* *) (function { get'; set'; cont'; clear' } -> begin set' 2 (get'(0) ^ get'(1)); cont' (); clear'(2) end);
      fmode [bb; ff; bb] (write_cost 4) (* *) (function { get'; set'; cont'; clear' } -> begin
	let body = get' (2) in
	let body_len = String.length body in
	let lhs = get' (0) in
	let lhs_len = String.length lhs in
	if lhs_len < body_len
	then if lhs = String.sub body 0 lhs_len
	  then begin
	    set' 1 (String.sub body lhs_len (body_len - lhs_len));
	    cont' ();
	    clear' (1)
	  end end);
      fmode [ff; bb; bb] (write_cost 4) (* *) (function { get'; set'; cont'; clear' } -> begin
	let body = get' (2) in
	let body_len = String.length body in
	let rhs = get' (1) in
	let rhs_len = String.length rhs in
	if rhs_len < body_len
	then if rhs = String.sub body (body_len - rhs_len) rhs_len
	  then begin
	    set' 0 (String.sub body 0 (body_len - rhs_len));
	    cont' ();
	    clear' (0)
	  end end);
      fmode [ff; ff; bb] (write_cost 4) (* *) (function { get'; set'; cont'; clear' } -> begin
	let body = get' (2) in
	let body_len = String.length body in
	for i = 0 to body_len do
	  set' 0 (String.sub body 0 i);
	  set' 1 (String.sub body i (body_len - i));
	  cont' ();
	  clear' (0); clear' (1)
	done
      end);
    ]

    let length = register "sys-length" 1 ["length"] [
      fmode [bb; ff] (write_cost 3) (* *) (function { get'; set'; cont'; clear'} -> begin set'(1) (string_of_int (String.length(get'(0)))); cont' (); clear'(1) end);
      fmode [bb; bb] (write_cost 3) (* *) (function { get'; set'=_; cont'; clear'=_} -> begin if get'(1) = (string_of_int (String.length(get'(0)))) then cont' () end);
    ]

    open Num
    let aton = num_of_string
    let ntoa = string_of_num

    let zero = num_of_int 0

    let add = register "sys-add" 2 ["sum"] [
      fmode [bb; bb; bb] (write_cost 5) (* *) (function { get'; set'=_; cont'; clear'=_} -> begin if get'(2) = ntoa ((aton (get'(0))) +/ (aton (get'(1)))) then cont' () end);
      fmode [bb; bb; ff] (write_cost 4) (* *) (function { get'; set'; cont'; clear'} -> begin set'(2) (ntoa (aton (get'(0)) +/ aton (get'(1)))); cont' (); clear'(2) end);
      fmode [bb; ff; bb] (write_cost 4) (* *) (function { get'; set'; cont'; clear'} -> begin set'(1) (ntoa (aton (get'(2)) -/ aton (get'(0)))); cont' (); clear'(1) end);
      fmode [ff; bb; bb] (write_cost 4) (* *) (function { get'; set'; cont'; clear'} -> begin set'(0) (ntoa (aton (get'(2)) -/ aton (get'(1)))); cont' (); clear'(0) end);
    ]

    let sub = register "sys-sub" 2 ["difference"] [
      fmode [bb; bb; bb] (write_cost 5) (* *) (function { get'; set'=_; cont'; clear'=_} -> begin if get'(2) = ntoa ((aton (get'(0))) -/ (aton (get'(1)))) then cont' () end);
      fmode [bb; bb; ff] (write_cost 4) (* *) (function { get'; set'; cont'; clear'} -> begin set'(2) (ntoa (aton (get'(0)) -/ aton (get'(1)))); cont' (); clear'(2) end);
      fmode [bb; ff; bb] (write_cost 4) (* *) (function { get'; set'; cont'; clear'} -> begin set'(1) (ntoa (aton (get'(0)) -/ aton (get'(2)))); cont' (); clear'(1) end);
      fmode [ff; bb; bb] (write_cost 4) (* *) (function { get'; set'; cont'; clear'} -> begin set'(0) (ntoa (aton (get'(2)) +/ aton (get'(1)))); cont' (); clear'(0) end);
    ]

    let do_div (process_quotient) { get'; set'=_; cont'=_; clear'=_ } (divident) (divisor) =
      let divisor = aton (get'(divisor)) in
      if divisor <>/ zero then
	let divident = aton (get'(divident)) in
	process_quotient (divident // divisor)

    let assign_div quotient ({ get'=_; set'; cont'; clear' } as context) =
      do_div (function q -> begin set' quotient (ntoa q); cont' (); clear' quotient end) context

    let check_div quotient ({ get'; set'=_; cont'; clear'=_ } as context) =
      do_div (function q -> begin if aton (get' quotient) =/ q then cont' () end) context

    let mul = register "sys-mul" 2 ["product"] [
      fmode [bb; bb; bb] (write_cost 5) (* *) (function { get'; set'=_; cont'; clear'=_} -> begin if get'(2) = ntoa ((aton (get'(0))) */ (aton (get'(1)))) then cont' () end);
      fmode [bb; bb; ff] (write_cost 4) (* *) (function { get'; set'; cont'; clear'} -> begin set'(2) (ntoa (aton (get'(0)) */ aton (get'(1)))); cont' (); clear'(2) end);
      fmode [bb; ff; bb] (write_cost 4) (* *) (function c -> assign_div 1 c 2 0);
      fmode [ff; bb; bb] (write_cost 4) (* *) (function c -> assign_div 0 c 2 1);
    ]

    let div = register "sys-div" 2 ["quotient"] [
      fmode [bb; bb; bb] (write_cost 5) (* *) (function c -> check_div 2 c 0 1);
      fmode [bb; bb; ff] (write_cost 4) (* *) (function c -> assign_div 2 c 0 1);
      fmode [bb; ff; bb] (write_cost 4) (* *) (function c -> assign_div 1 c 0 2);
      fmode [ff; bb; bb] (write_cost 4) (* *) (function { get'; set'; cont'; clear'} -> begin set'(0) (ntoa (aton (get'(2)) */ aton (get'(1)))); cont' (); clear'(0) end);
    ]

    let modulo = register "sys-modulo" 2 ["remainder"] [
      fmode [bb; bb; bb] (write_cost 5) (* *) (function { get'; set'=_; cont'; clear'=_} -> begin
	let divisor = aton (get'(1))
	in if divisor >/ zero
	  then if get'(2) = ntoa (mod_num (aton (get'(0))) divisor) then cont' ()
      end);
      fmode [bb; bb; ff] (write_cost 4) (* *) (function { get'; set'; cont'; clear'} -> begin
	let divisor = aton (get'(1))
	in if divisor >/ zero
	  then begin set'(2) (ntoa (mod_num (aton (get'(0))) (aton (get'(1))))); cont' (); clear'(2) end
      end);
(* This next one requires prime factor analysis, so we're not doing it for now. *)
(*
      fmode [bb; ff; bb] (write_cost 4) (* *) (function { get'; set'; cont'; clear'} -> begin set'(1) (ntoa (aton (get'(2)) -/ aton (get'(0)))); cont' (); clear'(1) end);
*)
    ]

    let lt = register "sys-lt" 2 [] [
      fmode [bb; bb] (write_cost 3) (* *) (function { get'; set'=_; cont'; clear'=_} -> begin if (aton (get'(0))) </ (aton (get'(1))) then cont' () end);
    ]

    let gt = register "sys-gt" 2 [] [
      fmode [bb; bb] (write_cost 3) (* *) (function { get'; set'=_; cont'; clear'=_} -> begin if (aton (get'(0))) >/ (aton (get'(1))) then cont' () end);
    ]

    let le = register "sys-le" 2 [] [
      fmode [bb; bb] (write_cost 3) (* *) (function { get'; set'=_; cont'; clear'=_} -> begin if (aton (get'(0))) <=/ (aton (get'(1))) then cont' () end);
    ]

    let ge = register "sys-ge" 2 [] [
      fmode [bb; bb] (write_cost 3) (* *) (function { get'; set'=_; cont'; clear'=_} -> begin if (aton (get'(0))) >=/ (aton (get'(1))) then cont' () end);
    ]

  end

let primops = Array.of_list (List.rev !primops_list)
let primops_names = Array.of_list (List.rev !primops_names)

let get = Array.get primops
let get_name = Array.get primops_names
let resolve = Hashtbl.find primops_table
let name_is_primop = Hashtbl.mem primops_table

let register () = () (* hide the primop_interface name.  FIXME: do this in mli. *)
