#include <stdio.h>
#include <string.h>

#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>

#include "exports.h"

// Makes a string list given a char* array
value makeStringList(const char **list, int n)
{
   CAMLparam0();
   CAMLlocal3(result, str, tmp);
   int i;
   for (i = n - 1; i >= 0; i--)
   {
      str = caml_copy_string(list[i]);
      tmp = caml_alloc(2, 0);
      Store_field(tmp, 0, str);
      Store_field(tmp, 1, result);
      result = tmp;
   }

   CAMLreturn(result);
}

void initializeOcaml(void)
{
   static int initialized = 0;
   char *args = NULL;
   if (initialized == 0)
   {
      caml_startup(&args);
      initialized = 1;
   }
}

EXPORT char *generateLuaCode(const char **files, int n)
{
   CAMLparam0();
   CAMLlocal2(ofiles, oresult);
   char *result = NULL;
   initializeOcaml();

   ofiles = makeStringList(files, n);

   static value *closure_f = NULL;
   if (closure_f == NULL)
   {
      closure_f = caml_named_value("generateLuaCode");
   }

   oresult = caml_callback(*closure_f, ofiles);
   result = strdup(String_val(oresult));
   CAMLreturnT(char *, result);
}