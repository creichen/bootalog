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

#include <check.h>

#include "table.h"

START_TEST (test_table_create)
{
    table_t *table = ntt_create_table(3, 7);
    fail_unless(table->rows_per_chunk == 7);
    fail_unless(table->columns_nr == 3);
    fail_unless(table->first_chunk != NULL);
    fail_unless(table->first_chunk == table->last_chunk);

    ntt_delete_table(table);
}
END_TEST


START_TEST (insert_grow)
{
    table_t *table = ntt_create_table(3, 2);

    fail_unless(table->first_chunk == table->last_chunk);
    fail_unless(table->first_chunk->prev_chunk == NULL);
    fail_unless(table->first_chunk->next_chunk == NULL);
    fail_unless(table->first_chunk->occupancy == 0);

    ntt_insert_row(table);

    fail_unless(table->first_chunk == table->last_chunk);
    fail_unless(table->first_chunk->prev_chunk == NULL);
    fail_unless(table->first_chunk->next_chunk == NULL);
    fail_unless(table->first_chunk->occupancy == 1);

    ntt_insert_row(table);

    fail_unless(table->first_chunk == table->last_chunk);
    fail_unless(table->first_chunk->prev_chunk == NULL);
    fail_unless(table->first_chunk->next_chunk == NULL);
    fail_unless(table->first_chunk->occupancy == 2);

    ntt_insert_row(table);

    fail_unless(table->first_chunk != table->last_chunk);
    fail_unless(table->first_chunk->prev_chunk == NULL);
    fail_unless(table->first_chunk->next_chunk == table->last_chunk);
    fail_unless(table->last_chunk->prev_chunk == table->first_chunk);
    fail_unless(table->last_chunk->next_chunk == NULL);
    fail_unless(table->first_chunk->occupancy == 2);
    fail_unless(table->last_chunk->occupancy == 1);

    ntt_insert_row(table);
    ntt_insert_row(table);

    const chunk_t * middle_chunk = table->first_chunk->next_chunk;

    fail_unless(table->first_chunk != table->last_chunk);
    fail_unless(table->first_chunk != middle_chunk);
    fail_unless(middle_chunk != table->last_chunk);

    fail_unless(table->first_chunk->prev_chunk == NULL);
    fail_unless(table->first_chunk->next_chunk == middle_chunk);
    fail_unless(middle_chunk->prev_chunk == table->first_chunk);
    fail_unless(middle_chunk->next_chunk == table->last_chunk);
    fail_unless(table->last_chunk->prev_chunk == middle_chunk);
    fail_unless(table->last_chunk->next_chunk == NULL);
    fail_unless(table->first_chunk->occupancy == 2);
    fail_unless(table->last_chunk->occupancy == 1);

    ntt_delete_table(table);
}
END_TEST


START_TEST (zero_size_table)
{
    table_t *table = ntt_create_table(3, 0);

    fail_unless(table->rows_per_chunk == 0);
    fail_unless(!table->first_chunk);
    fail_unless(!table->last_chunk);

    // Can recover from poor space heuristic
    ntt_insert_row(table);

    fail_unless(table->rows_per_chunk > 0);
    fail_unless(table->first_chunk != NULL);
    fail_unless(table->last_chunk != NULL);

    ntt_delete_table(table);
}
END_TEST


START_TEST (insert_delete_small)
{
    int a, b, c;

    table_t *table = ntt_create_table(3, 4);
 
    ntt_insert_row(table)[0] = &a;
    ntt_insert_row(table)[0] = &b;
    ntt_insert_row(table)[0] = &c;

    fail_unless(table->first_chunk->occupancy == 3);
    fail_unless(table->first_chunk->data[1 * 3] == &b);

    ntt_delete_row(table, table->first_chunk, 1);

    fail_unless(table->first_chunk->occupancy == 2);
    fail_unless(table->first_chunk->data[0 * 3] == &a);
    fail_unless(table->first_chunk->data[1 * 3] == &c);

    ntt_delete_row(table, table->first_chunk, 1);
    fail_unless(table->first_chunk->occupancy == 1);
    fail_unless(table->first_chunk->data[0 * 3] == &a);

    ntt_delete_table(table);
}
END_TEST

START_TEST (grow_shrink)
{
    table_t *table = ntt_create_table(3, 2);
    int i;

    for (i = 0; i < 6; i++) {
	ntt_insert_row(table);
    }

    // (at least) three chunks
    fail_unless(table->first_chunk->next_chunk != table->last_chunk);

    ntt_delete_row(table, table->last_chunk, 1);
    ntt_delete_row(table, table->last_chunk, 0);
    ntt_delete_row(table, table->first_chunk->next_chunk, 1);

    // two chunks
    fail_unless(table->first_chunk->next_chunk == table->last_chunk);

    ntt_delete_row(table, table->first_chunk->next_chunk, 0);
    ntt_delete_row(table, table->first_chunk, 1);

    // one chunk
    fail_unless(table->first_chunk == table->last_chunk);

    ntt_delete_table(table);
}
END_TEST

START_TEST (delete_middle)
{
    int a, b, c, d, e, f;

    table_t *table = ntt_create_table(3, 2);

    ntt_insert_row(table)[0] = &a;
    ntt_insert_row(table)[0] = &b;
    ntt_insert_row(table)[0] = &c;
    ntt_insert_row(table)[0] = &d;
    ntt_insert_row(table)[0] = &e;
    ntt_insert_row(table)[0] = &f;

    ntt_delete_row(table, table->first_chunk->next_chunk, 0);
    fail_unless(table->last_chunk->occupancy == 1);
    fail_unless(table->first_chunk->next_chunk->data[0] == &f);

    ntt_delete_table(table);
}
END_TEST

void
add_test(Suite *s, char *name, void (*fn)(int))
{
    TCase *tc = tcase_create(name);
    tcase_add_test(tc, fn);
    suite_add_tcase(s, tc);
}

Suite *
test_suite()
{
    Suite *s = suite_create("Table");

    add_test(s, "test-table-create", test_table_create);
    add_test(s, "insert-grow", insert_grow);
    add_test(s, "zero-size-table", zero_size_table);
    add_test(s, "insert-delete-small", insert_delete_small);
    add_test(s, "grow-shrink", grow_shrink);
    add_test(s, "delete-middle", delete_middle);

    return s;
}

int
main(int argc, char **argv)
{
    int number_failed;
    Suite *s = test_suite();
    SRunner *sr = srunner_create(s);
    srunner_run_all(sr, CK_NORMAL);
    number_failed = srunner_ntests_failed(sr);
    srunner_free(sr);
    return (number_failed == 0) ? 0 : 1;
}
