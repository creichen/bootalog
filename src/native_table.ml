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

 The author can be reached as "creichen" at the usual gmail server.

***************************************************************************)

(*  *)

open Base

type ptr

type t
type cursor

let name = "native"

external from_ptr : ptr -> t = "%identity"
external to_ptr : t -> ptr = "%identity"

module Internal =
struct
  type cursor = { mutable chunk : ptr; mutable index : int; stride : int }

  (* external get_null_ptr : unit -> unit = "caml_get_null" *)
  (* let null_ptr = get_null_ptr () *)

  (* Create fresh, empty table *)
  external create :
    int (* columns *) ->
      int (* number of entries to preallocate *) ->
	t = "caml_table_create"

  (*
    Drop a table (does not touch the 
    Note that C-heap tables don't have their own schema information, so they can't delete their own records.
  *)
  external delete : t -> unit = "caml_table_delete"

  (* Allows duplicate insertions! *)
  external insert : t -> atom array -> unit = "caml_table_insert"

  external cursor_find : t -> atom array -> cursor = "caml_table_find"
  external cursor_is_finished : cursor -> bool = "caml_table_cursor_is_finished"
  external cursor_head : t -> cursor = "caml_table_cursor_head"
  external cursor_next : cursor -> unit = "caml_table_cursor_next"
  external cursor_get : cursor -> atom array = "caml_table_cursor_get"
  (* remove current row and update cursor to next *)
  external cursor_remove : t -> cursor -> unit = "caml_table_cursor_remove"

  let iter (table) (apply_fn) =
    let cursor = cursor_head (table)
    in while not (cursor_is_finished (cursor)) do
	apply_fn (cursor_get (cursor));
        cursor_next (cursor);
      done
end

let drop (table : t) : unit =
  (* FIXME: find and delete inner tables (* or should this rather happen via GC?  Let's do GC. *) *)
  Internal.delete table

let contains (table : t) ((labels, values) : tuple) : bool =
  begin
    Label.order_labels values labels;
    not (Internal.cursor_is_finished (Internal.cursor_find table values))
  end

let insert = Internal.insert
let insert' table tuple = insert table (Array.of_list tuple)

let show (_) = "<show function for native table not implemneted yet>"

(*
FIXME: need to adjust because Internal.iter doesn't provide labels; i.e., we need to have a schema
  table before we can make use of this.
let bind_all = Table_support.bind_all (Internal.iter) (contains)
*)
