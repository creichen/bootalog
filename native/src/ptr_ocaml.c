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

#include <stdio.h>
#include <string.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

CAMLprim value
caml_ptr_compare(value v1, value v2)
{
    fprintf(stderr, "%% PTR compare...");
    return v2 - v1;
}


CAMLprim value
caml_ptr_show(value v)
{
    CAMLparam1( v );
    CAMLlocal1( retval );
    char s[64];
    size_t str_len = snprintf(s, 64, "%p", v);
    s[63] = 0;

    retval = caml_alloc_string(str_len);
    memcpy(String_val(retval), s, str_len);
    CAMLreturn(retval);
}
