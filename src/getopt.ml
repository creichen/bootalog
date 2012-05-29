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

open Printf;;

type 'a opt_proc = NoArg	of ('a -> 'a)
	         | WithArg	of string * ('a * string -> 'a);;

(** The final string here is a brief description of the parameter *)
type 'a opt = char option * string * 'a opt_proc * string;;


(* Note to self: this might have been easier with the `Format' module *)
let print_help (print) (optlist : 'a opt list) =
  let extract_maxlen (extractor) =
    let rec m (optlist) = match optlist with
	[]	-> 0
      | (h::tl)	-> (match extractor (h) with
		         None	-> m (tl)
		       | Some s	-> let r0 = String.length (s) in
				   let r1 = m (tl) in
				   if r0 > r1 then r0 else r1
      )
    in m optlist
  in let (long_len, arg_len) = let extract_longarg (_, longarg, _, _) = Some longarg in
			       let extract_optname (_, _, proc, _) = match proc with
				   NoArg _	 	-> None
				 | WithArg (s, _)	-> Some s
			       in (extract_maxlen (extract_longarg),
				   extract_maxlen (extract_optname))
     in let pad l s = sprintf "%-*s" l s in
	let print_char c = print (sprintf "%c" c)
	in let print_opt (shortopt, long, proc, descr) =
	     begin
	       print (" ");
	       (match shortopt with
		   None	-> print "  "
		 | Some c	-> (print_char '-'; print_char c));
	       print ("  --");
	       print (pad long_len long);
	       print (" ");
	       print (pad (arg_len + 4)
			       (match proc with
				   NoArg _		-> ""
				 | WithArg (s, _)	-> ("(" ^ s ^ ")")));
	       print descr;
	       print "\n";
	     end
	   in let compare_oli (_, a, _, _) (_, b, _, _) = a < b
	   in List.iter print_opt (Sort.list compare_oli optlist);;

exception Fail;;
exception Arg_fail;;

let process_args error_handler (optlist : 'a opt list) default_value argv =
  try
    let fail (s : string) = (error_handler s; raise Fail) in
    let result = ref default_value in
    let ignored_args : string list ref = ref [] in
    let double_dash_mode = ref false in

    let find (errhandler, p) =
      try List.find p (optlist)
      with Not_found -> fail errhandler
    in
    let find_long s = let check_long (_, s', _, _) = s = s'
		      in find ("Unknown option `--" ^ s ^"'",
			       check_long) in
    let find_short c = let check_short (c', _, _, _) = Some c = c'
		       in find (sprintf "Unknown option `-%c'" c,
				check_short) in

    let i = ref 1 in (
    while !i < Array.length argv
    do let arg = Array.get argv !i in
       begin
	 i := 1 + !i;
	 if !double_dash_mode
	 then ignored_args := arg :: !ignored_args (* skip all remaining parameters as non-options *)
	 else if arg = "--"
	 then double_dash_mode := true
	 else
	   let is_option = String.length (arg) > 0
	                && String.get (arg) 0 == '-'
	   in if not is_option
	     then ignored_args := arg :: !ignored_args (* Collect non-options *)
	     else let is_long = String.length (arg) > 1
	                     && String.get (arg) 1 == '-'
		             && not (!double_dash_mode) in
		  let get_extra_arg_for (option) =
		    if !i < Array.length (argv)
		    then let result = Array.get (argv) !i
			 in (i := 1 + !i;
			     result)
		    else fail (sprintf "Missing argument to option `%s'" option) in

		  let handle_option (proc, arg, optname, get_arg) =
		    match proc, arg with
			NoArg f, Some a	-> fail (sprintf "Unexpected argument `%s' to option `%s'" a optname)
		      | NoArg f, None		-> result := f (!result)
		      | WithArg (_, f),Some a	-> result := f (!result, a)
		      | WithArg (_, f), None	-> result := f (!result, get_arg (optname))

		  in if is_long
		    then let (optname,
			      builtin_arg) = let opt = Str.string_after arg 2 in
					     try let sep = String.index opt '=' in
						 (Str.string_before opt sep,
						  Some (Str.string_after opt (sep + 1)))
					     with Not_found -> (opt, None) in
			 let (_, _, proc, _) = find_long (optname)
			 in handle_option (proc, builtin_arg, (sprintf "--%s" optname), get_extra_arg_for)

		    else (* not long, may be multiple options *)
		      let rec process_option (body) =
			if String.length (body) > 0
			then let (opt_char,
				  (assign_body,
				   rest)) = (String.get body 0,
					     if String.length (body) > 1 && String.get (body) 1 == '='
					     then (Some (Str.string_after body 2), "")
					     else (None, Str.string_after body 1)) in
			     let (_, _, proc, _) = find_short (opt_char) in
			     let orest = ref rest in
			     let get_arg_next (optname) =
			       if String.length (rest) > 0
			       then (orest := "";
				     rest)
			       else get_extra_arg_for (optname) in
			     begin
			       handle_option (proc, assign_body, (sprintf "-%c" opt_char), get_arg_next);
			       process_option (!orest)
			     end
		      in process_option (Str.string_after arg 1)
       end
    done;
      (!result, List.rev !ignored_args)
    )
  with Fail -> (raise Arg_fail);;

let process_commandline error_handler optlist default_value =
  process_args error_handler optlist default_value Sys.argv;;
