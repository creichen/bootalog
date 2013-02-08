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

type t

type ptr  (* C pointer; used both for tables (Native_table.t) and for atoms (Atom.t). *)
external show_ptr : ptr -> string = "caml_ptr_show"

external from_ptr : ptr -> t = "%identity"
external to_ptr : t -> ptr = "%identity"

external from_string : string -> t = "caml_hc_hashcons"
external to_string : t -> string = "caml_hc_extract_string"

let show = to_string
module Internal =
struct
  external init : unit -> unit = "caml_hc_init"
end
let () = Internal.init()
let dummy = from_string("")

external compare : t -> t -> int = "caml_ptr_compare"
