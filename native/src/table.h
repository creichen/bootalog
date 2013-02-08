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
// Unbounded table with the number of columns specified at creation time.
// Inserts and deletes are O(1).
// Lookups are O(n).
// Main design principle was simplicity; this table is intended as a placeholder
// for more sophisticated datatype usage.


#ifndef BOOTALOG_NATIVE_TABLE_H_
#define BOOTALOG_NATIVE_TABLE_H_

#define NO_FIRST_FREE -1

typedef struct chunk {
    int occupancy;
    struct chunk *prev_chunk;
    struct chunk *next_chunk;
    void *data[];
} chunk_t;

// For now our simple tables are completely unordered
typedef struct {
    int rows_per_chunk;
    int columns_nr;
    chunk_t *first_chunk;
    chunk_t *last_chunk;
} table_t;

#define CELL_SIZE sizeof(void *)

table_t *
ntt_create_table(int columns_nr, int expected);

// Doesn't deallocate any "inner data" objects
void
ntt_delete_table(table_t *table);

void
ntt_delete_chunk_and_descendants(table_t *table, chunk_t *chunk);

void **
ntt_insert_row(table_t *table);

void
ntt_delete_row(table_t *table, chunk_t *chunk, int index);

#endif  // !defined(BOOTALOG_NATIVE_TABLE_H_)
