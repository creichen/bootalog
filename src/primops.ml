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

open Primop_interface
(* Explicitly include any primop sub-libraries here! *)

type primop_id = int

type primop = string * (access_mode list)

type adapter_interface =
  { get	 : int -> Base.atom;
    set  : int -> Base.atom -> unit;
    cont : unit -> unit }

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

let mode m c e = {
  variable_modes	= m;
  evaluator		= adapter e;
  cost			= c
}

let min_cost = { cost_simple = 1 }
let write_cost n = { cost_simple = 3 * n }

let bb = Bound
let ff = Free

module Sys =
  struct
    let eq = register "=" [
      mode [bb; ff] (write_cost 1) (* *) (function { get; set; cont } -> begin set 1 (get 0); cont () end);
      mode [ff; bb] (write_cost 1) (* *) (function { get; set; cont } -> begin set 0 (get 1); cont () end);
      mode [bb; bb] min_cost	   (* *) (function { get; set=_; cont } -> begin if get 0 = get 1 then cont () end)
    ]
  end

let primops = Array.of_list (List.rev !primops_list)
let primops_names = Array.of_list (List.rev !primops_names)

let get = Array.get primops
let get_name = Array.get primops_names
let resolve = Hashtbl.find primops_table

let register () = () (* hide the primop_interface name.  FIXME: do this in mli. *)
