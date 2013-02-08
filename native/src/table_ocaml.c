/***************************************************************************
  Copyright (C) 2013 Christoph Reichenbach


 This program may be modified and copied freely according to the terms of
 the GNU general public license (GPL), as long as the above copyright
 notice and the licensing information contained herein are preserved.

 Please refer to www.gnu.org for licensing details.

 This work is provided AS IS, without warranty of any kind, expressed or
 implied, including but not limited to the warranties of merchantability,
 noninfringement, and fitness for a specific purpose. The author will not
 be held liable for any damage caused by this work or derivatives of it.

 By using this source code, you agree to the licensing terms as stated
 above.


 Please contact the maintainer for bug reports or inquiries.

 Current Maintainer:

    Christoph Reichenbach (CR) <creichen@gmail.com>

***************************************************************************/

#include "table.h"

#include <stdio.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

CAMLprim value
caml_get_null(value unit)
{
    fprintf(stderr, "%%  %s...\n", __FUNCTION__);
    return (value) NULL;
}

CAMLprim value
caml_table_create(value columns, value default_entries)
{
    fprintf(stderr, "%%  %s...\n", __FUNCTION__);
    return (value) ntt_create_table(Int_val(columns), Int_val(default_entries));
}

CAMLprim value
caml_table_delete(value table)
{
    fprintf(stderr, "%%  %s...\n", __FUNCTION__);
    ntt_delete_table((table_t *) table);
    return Val_unit;
}

CAMLprim value
caml_table_insert(value table, value tuple_as_array)
{
    fprintf(stderr, "%%  %s...\n", __FUNCTION__);
    const int size = Wosize_val(tuple_as_array);
    int i;
    void **record = ntt_insert_row((table_t *) table);

    for (i = 0; i < size; i++) {
	record[i] = (void *) Field(tuple_as_array, i);
    }
    return Val_unit;
}

CAMLprim value
caml_table_find(value proto_table, value tuple_as_array)
{
    fprintf(stderr, "%%  %s...\n", __FUNCTION__);
    // linear scan
    CAMLparam1( tuple_as_array );
    CAMLlocal1( cursor );
    const table_t *table = (table_t *) proto_table;
    chunk_t *chunk = table->first_chunk;
    const int size = Wosize_val(tuple_as_array);
    cursor = caml_alloc(3, 0);
    Store_field(cursor, 2, Int_val(table->columns_nr));

    while (chunk) {
	int index;
	for (index = 0; index < chunk->occupancy; ++index) {
	    void **data = &(chunk->data[index]);
	    int i;
	    for (i = 0; i < size; i++) {
		if (data[i] != (void *)Field(tuple_as_array, i))
		    goto not_found;

		// Found at chunk and index
                Store_field(cursor, 0, (value) chunk);
		Store_field(cursor, 1, Int_val(index));
		CAMLreturn(cursor);
	    }
	not_found:;
	}
	chunk = chunk->next_chunk;
    }
    Store_field(cursor, 0, (value) NULL);
    Store_field(cursor, 1, Int_val(0));
    CAMLreturn(cursor);
}

CAMLprim value
caml_table_cursor_is_finished(value cursor)
{
    fprintf(stderr, "%%  %s...\n", __FUNCTION__);
    return Bool_val(NULL == (void *)Field(cursor, 0));
}

CAMLprim value
caml_table_cursor_head(value proto_table)
{
    fprintf(stderr, "%%  %s...\n", __FUNCTION__);
    CAMLparam1( proto_table );
    CAMLlocal1( cursor );
    const table_t *table = (table_t *) proto_table;
    cursor = caml_alloc(3, 0);

    Store_field(cursor, 0, (value) table->first_chunk);
    Store_field(cursor, 1, Int_val(0));
    Store_field(cursor, 2, Int_val(table->columns_nr));

    if (table->first_chunk
	&& !table->first_chunk->occupancy) {
	Store_field(cursor, 0, (value) NULL);
    }
    CAMLreturn(cursor);
}

CAMLprim value
caml_table_cursor_next(value cursor)
{
    fprintf(stderr, "%%  %s...\n", __FUNCTION__);
    CAMLparam1( cursor );
    const chunk_t *chunk = (chunk_t *) Field(cursor, 0);
    const int index = Int_val(Field(cursor, 1));

    if (index + 1 < chunk->occupancy) {
	Store_field(cursor, 1, Int_val(index + 1));
    } else {
	Store_field(cursor, 0, (value) chunk->next_chunk);
	Store_field(cursor, 1, Int_val(0));
    }
    return Val_unit;
}

CAMLprim value
caml_table_cursor_get(value table, value cursor)
{
    fprintf(stderr, "%%  %s...\n", __FUNCTION__);
    CAMLparam2( table, cursor );
    CAMLlocal1( tuple );
    const chunk_t *chunk = (chunk_t *) Field(cursor, 0);
    const int index = Int_val(Field(cursor, 1));
    const int columns_nr = Int_val(Field(cursor, 2));
    const int offset = index * columns_nr;
    int i;

    if (!chunk) {
	caml_failwith("INTERNAL ERROR (caml_table_cursor_get): attempted use of NULL chunk as cursor\n");
    }

    tuple = caml_alloc(columns_nr, 0);
    for (i = 0; i < columns_nr; i++) {
	Store_field(tuple, i, chunk->data[offset + i]);
    }

    CAMLreturn(tuple);
}

CAMLprim value
caml_table_cursor_remove(value table, value cursor)
{
    fprintf(stderr, "%%  %s...\n", __FUNCTION__);
    CAMLparam2( table, cursor );
    chunk_t *chunk = (chunk_t *) Field(cursor, 0);
    const int index = Int_val(Field(cursor, 1));

    if (!chunk) {
	caml_failwith("INTERNAL ERROR (caml_table_cursor_remove)): attempted use of NULL chunk as cursor\n");
    }

    ntt_delete_row((table_t *) table, chunk, index);

    return Val_unit;
}
