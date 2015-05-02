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

#include "istrings.h"

#include <strings.h>

typedef struct isi_string {
  unsigned long hash;
  unsigned int size;
  unsigned int count;
  struct isi_string *next; // spill list
  char data[];
} isi_string_t;

static int isi_table_size = 0;

static isi_string_t *isi_table = NULL;

static unsigned long
hash(char *src, int length)
{
    int hashcode = 17;
    long *data = (long *) src;

    // FVN-1 style hash
    while (length >= sizeof(long)) {
	hashcode *= 43;
	hashcode ^= *data;
	data++;
	length -= sizeof(long);
    }
    src = (char *) data;
    while (length > 0) {
	hashcode *= 23;
	hashcode ^= data[length--];
    }
    return hashcode;
}

char *
isi_intern_string(char *src, int length);

// For interned strings only
void
isi_inc_refcount(char *str);

void
isi_dec_refcount(char *str);

void
isi_clear_unused();


