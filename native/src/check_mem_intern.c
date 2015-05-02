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

#include "mem_intern.h"

#include <check.h>

START_TEST (test_idempotence)
{
    int data = 42;
    fail_unless(hc_hashcons(&data, sizeof(int))
		== hc_hashcons(&data, sizeof(int)));
    fail_unless(hc_hashcons(&data, sizeof(int)) != &data);
}
END_TEST


START_TEST (test_value_equality)
{
    int data = 42;
    fail_unless(*((int *)hc_hashcons(&data, sizeof(int)))
		== data);
}
END_TEST


START_TEST (test_size)
{
    int data = 42;
    hc_void *ptr = hc_hashcons(&data, sizeof(int));
    fail_unless(hc_size(ptr) == sizeof(int));
    fail_unless(ptr != &data);

    char *string = "And now for something completely different.";
    ptr = hc_hashcons(string, strlen(string));
    fail_unless(hc_size(ptr) == strlen(string));
    fail_unless(ptr != string);
}
END_TEST

void
test_equals_at_size(size_t size)
{
    char *data = malloc(size);
    char *copy = malloc(size);
    int i;

    for (i = 0; i < size; i++) {
	data[i] = i & 0xff;
    }

    hc_void *data_ptr = hc_hashcons(data, size);
    fail_unless(0 == memcmp(data_ptr, data, size));
    memcpy(copy, data, size);
    fail_unless(0 == memcmp(copy, data, size));
    fail_unless(hc_hashcons(copy, size) == data_ptr);
    fail_unless(hc_hashcons(data_ptr, hc_size(data_ptr)) == data_ptr);

    for (i = 0; i < size; i++) {
	copy[i] = ~copy[i];
	/* fprintf(stderr, "%p: size=%d, offset=%i, b=%02x vs. %02x\n", */
	/* 	copy, size, i, copy[i] & 0xff, data[i]); */
	fail_unless(hc_hashcons(copy, size) != data_ptr);
	copy[i] = ~copy[i];
	fail_unless(hc_hashcons(copy, size) == data_ptr);
    }

    free(data);
    free(copy);
}

START_TEST (test_equals)
{
    test_equals_at_size(0);

    int i, j;

    for (i = 0; i < 12; i++) {
	for (j = 0; j < 8; j++) {
	    test_equals_at_size((1 << i) + j);
	}
    }
}
END_TEST

/*
// Test disabled:  we make the assumption that everything is
// sizeof(long)-aligned.
START_TEST (test_stable_hash)
{
    char *data0 = "12345678abcdefgh";
    char buffer[64];

    hc_void *interned_data0 = hc_hashcons(data0, strlen(data0));

    for (int offset = 0; offset < 16; offset++) {
	memcpy(buffer + offset, data0, strlen(data0));
	fail_unless(hc_hashcons(buffer + offset, strlen(data0))
		    == interned_data0);
    }
}
END_TEST
*/

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
    Suite *s = suite_create("MemIntern");

    add_test(s, "idempotence", test_idempotence);
    add_test(s, "value-equality", test_value_equality);
    add_test(s, "size", test_size);
    add_test(s, "equals", test_equals);
    /* add_test(s, "stable-hash", test_stable_hash); */

    return s;
}

int
main(int argc, char **argv)
{
    int number_failed;
    Suite *s = test_suite();
    SRunner *sr = srunner_create(s);

    hc_init();
    srunner_run_all(sr, CK_NORMAL);
    number_failed = srunner_ntests_failed(sr);
    srunner_free(sr);

    return (number_failed == 0) ? 0 : 1;
}
