#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "exports.h"

int main(void)
{
   const char *files[] = {"../examples/voice.vult"};
   char *result = generateLuaCode(files, 1);
   int size = strlen(result);
   free(result);
   printf("Size of the result: %i\n", size);
   return 0;
}