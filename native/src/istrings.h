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

#ifndef BOOTALOG_NATIVE_ISTRINGS_H_
#define BOOTALOG_NATIVE_ISTRINGS_H_

char *
isi_intern_string(char *src, int length);

// For interned strings only
void
isi_inc_refcount(char *str);

void
isi_dec_refcount(char *str);

void
isi_clear_unused();

#endif  // !defined(BOOTALOG_NATIVE_ISTRINGS_H_)
