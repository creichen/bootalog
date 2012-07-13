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

open Base
open Program
module DB = Database

module DBFrontend =
struct
  let create () =
    DB.create (Combined_table.create)

  (* EDB insertion *)
  let add db ((predicate, tuple) : fact) =
    let insert pred tuple = Combined_table.insert (DB.get_table db pred) tuple
    in begin
      insert (Predicate.P predicate) tuple;
      Array.iter (function v -> insert Predicate.atom [|v|]) tuple
    end

  (* EDB removal *)
  let remove db ((predicate, tuple) : fact) =
    Combined_table.remove (DB.get_table db (Predicate.P predicate)) tuple

  let import db (facts : fact list) =
    List.iter (add db) facts
end
