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
caml_hc_init(value unit)
{
    hc_init();
    return Val_unit;
}

CAMLprim value
caml_hc_hashcons(value str)
{
    mlsize_t length = caml_string_length(str);
    const retval = (value) hc_hashcons(String_val(str), length);
    return retval;
}

CAMLprim value
caml_hc_size(value hash_str)
{
    fprintf(stderr, "%% Getting size...\n");
    return hc_size((hc_void *) hash_str);
}

CAMLprim value
caml_hc_extract_string(value hash_str)
{
    const int size = hc_size((hc_void *) hash_str);
    value v = caml_alloc_string(size);
    memcpy(String_val(v), (void *) hash_str, size);
    return v;
}
