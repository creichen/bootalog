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
// Memory hash-consing mechanism.
// Threadsafe.

#ifndef BOOTALOG_NATIVE_MEM_INTERN_H_
#define BOOTALOG_NATIVE_MEM_INTERN_H_

#include <stdlib.h>
#include <stdio.h>

typedef void hc_void;

// Must be called before any use
void
hc_init();

// Hash-cons a single data object's binary representation.
// Data pointer must be sizeof(long) aligned, or (depending on
// architecture) performance issues or exceptions may arise.
hc_void *
hc_hashcons(void *data, size_t length);

size_t
hc_size(hc_void *data);

#endif  // !defined(BOOTALOG_NATIVE_MEM_INTERN_H_)
