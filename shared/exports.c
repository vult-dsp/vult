#include <stdio.h>

#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>

#include "exports.h"

EXPORT void initializeOcaml(void)
{
   fflush(stdout);
   char *args = NULL;
   caml_startup(&args);
}
