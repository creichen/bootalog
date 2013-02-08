#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>

CAMLprim value
c_update_record(value to_insert, value to_update)
{
  Store_field(to_update, 1, to_insert);
  return Val_unit;
}
