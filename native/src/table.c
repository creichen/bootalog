/***************************************************************************
  Copyright (C) 2012 Christoph Reichenbach


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
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#define XALLOC(size) malloc(size);

chunk_t *
ntt_create_chunk(int cells_nr, chunk_t *prev)
{
    chunk_t *new_chunk = (chunk_t *) XALLOC(sizeof(chunk_t) + CELL_SIZE * cells_nr);
    new_chunk->prev_chunk = prev;
    new_chunk->next_chunk = NULL;
    new_chunk->occupancy = 0;
    if (prev) {
	assert(!prev->next_chunk);
	prev->next_chunk = new_chunk;
    }
    return new_chunk;
}

table_t *
ntt_create_table(int columns_nr, int expected)
{
    table_t *retval = XALLOC(sizeof(table_t));
    retval->rows_per_chunk = expected;
    retval->columns_nr = columns_nr;
    if (expected) {
	retval->first_chunk = ntt_create_chunk(columns_nr * expected, NULL);
    } else {
	retval->first_chunk = NULL;
    }
    retval->last_chunk = retval->first_chunk;
    return retval;
}

void
ntt_delete_table(table_t *table)
{
    ntt_delete_chunk_and_descendants(table, table->first_chunk);
    free(table);
}

void
ntt_delete_chunk_and_descendants(table_t *table, chunk_t *chunk)
{
    if (chunk == NULL) {
	return;
    }
    table->last_chunk = chunk->prev_chunk;
    if (table->first_chunk == chunk) {
	table->first_chunk = NULL;
    }
    if (chunk->prev_chunk) {
	chunk->prev_chunk->next_chunk = NULL;
    }
    while (chunk) {
	chunk_t *next = chunk->next_chunk;
	free(chunk);
	chunk = next;
    }
}

void **
ntt_insert_row(table_t *table)
{
    if (!table->rows_per_chunk) {
	table->rows_per_chunk = 4; // Evidently a bad heuristic
    }

    const int columns_nr = table->columns_nr;
    const int rows_per_chunk = table->rows_per_chunk;

    if (!table->last_chunk) {
	table->last_chunk = table->first_chunk = ntt_create_chunk(columns_nr * rows_per_chunk, NULL);
    }

    chunk_t *chunk = table->last_chunk;
    if (chunk->occupancy == rows_per_chunk) {
	chunk = table->last_chunk = ntt_create_chunk(columns_nr * rows_per_chunk, chunk);
    }

    return chunk->data + (columns_nr * chunk->occupancy++);
}

void
ntt_delete_row(table_t *table, chunk_t *chunk, int index)
{
    const int columns_nr = table->columns_nr;
    assert(index < chunk->occupancy);

    chunk_t *last_occupied_chunk = table->last_chunk;
    assert(last_occupied_chunk);
    while (!last_occupied_chunk->occupancy) {
	last_occupied_chunk = last_occupied_chunk->prev_chunk;
	assert(last_occupied_chunk);
    }

    // If we're deleting the last entry, then we will copy the entry on itself.
    // valgrind issues an error here, but this is harmless.
    memcpy(chunk->data + (index * columns_nr),
	   last_occupied_chunk->data + (--last_occupied_chunk->occupancy * columns_nr),
	   columns_nr * CELL_SIZE);

    if (last_occupied_chunk != table->last_chunk) {
	ntt_delete_chunk_and_descendants(table, last_occupied_chunk->next_chunk);
	table->last_chunk = last_occupied_chunk;
    }
}
