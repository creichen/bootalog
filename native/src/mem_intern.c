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

#include <pthread.h>
#include <stddef.h>
#include <string.h>


typedef unsigned long long hash_t;

typedef struct hash_entry {
    hash_t hash;
    struct hash_entry* next;  // next in spill list, if any
    size_t size;
    char data[];
} hash_entry_t;

static hash_t
hc_hash(void *data, size_t size)
// assume long alignment
{
    unsigned long *ld = data;
    hash_t hashcode = 43;

    while (size > sizeof(unsigned long)) {
	hashcode += *ld++;
	hashcode *= 23;
	size -= sizeof(unsigned long);
    }

    unsigned char *d = (unsigned char *)ld;

    while (size--) {
	hashcode += *d++;
	hashcode *= 17;
    }

    return hashcode;
}

static hash_entry_t *
hc_new_entry(const hash_t hashcode, const void *data, const size_t size)
{
    hash_entry_t *entry = malloc(sizeof(hash_entry_t) + size);

    entry->size = size;
    entry->next = NULL;
    entry->hash = hashcode;
    memcpy(&entry->data, data, size);
  
    return entry;
}

static volatile int hc_mutex_initialised = 0;  // heuristic safety mechanism to avoid multiple inits
static pthread_mutex_t mutex;

static int hc_table_mask = -1;  // We hash via &, and this variable carries the mask (1 less than table size.)
static int hc_table_occupancy = 0;
static hash_entry_t **hc_table = NULL;

void
hc_init()
{
    if (!hc_mutex_initialised) {
	hc_mutex_initialised = 1;
	pthread_mutex_init(&mutex, NULL);
    }
}

static inline int
hc_must_grow_table() {
    return hc_table_occupancy * 4 > hc_table_mask;
}

void hc_grow_table(int new_size) {
    int old_size = hc_table_mask + 1;
    hc_table_mask = new_size - 1;
    hash_entry_t **new_table = calloc(sizeof(hash_entry_t *), new_size);

    // re-sort all entries
    int old_offset;
    for (old_offset = 0; old_offset < old_size; old_offset++) {
	hash_entry_t *entry = hc_table[old_offset];
	while (entry) {
	    int new_offset = entry->hash & hc_table_mask;
	    hash_entry_t *next_entry = entry->next;
	    entry->next = new_table[new_offset];
	    new_table[new_offset] = entry;
	    entry = next_entry;
	}
    }

    if (hc_table) {
	free(hc_table);
    }
    hc_table = new_table;
}

hc_void *
hc_hashcons(void *data, size_t size)
{
    if (hc_table_mask < 0) {
	pthread_mutex_lock(&mutex);
	hc_grow_table(256);
	pthread_mutex_unlock(&mutex);
    }

    hash_entry_t *result;

    const hash_t hash = hc_hash(data, size);
    // lock
    pthread_mutex_lock(&mutex);
    int offset = hash & (hc_table_mask);

    hash_entry_t *entry = hc_table[offset];
    while (entry
	   && (entry->hash != hash
	       || entry->size != size
	       || memcmp(data, entry->data, size))) {
	entry = entry->next;
    }

    if (entry) {
	result = entry;
    } else {
	// have to allocate new cell
	result = hc_new_entry(hash, data, size);
	/* fprintf(stderr, "[NEW] %p: size=%d, code=%ll\n", */
	/* 	result, size, hash); */
	result->next = hc_table[offset];
	hc_table[offset] = result;
	++hc_table_occupancy;

	if (hc_must_grow_table()) {
	    hc_grow_table((hc_table_mask + 1) * 2);
	}
    }

    // unlock
    pthread_mutex_unlock(&mutex);

    return &(result->data);
}

size_t
hc_size(hc_void *data)
{
    const hash_entry_t *entry = (((char *)data) - offsetof(hash_entry_t, data));
    return entry->size;
}
