

#include "bench.h"
#include "vultin.h"
#include <time.h>
#include <stdio.h>

int main(void)
{
   Bench_process_type data;
   Bench_process_init(data);

   int time = 44100 * 200;

   clock_t start = clock(), diff;

   while (time > 0)
   {
      Bench_process(data);
      time--;
   }

   diff = clock() - start;

   int msec = diff * 1000 / CLOCKS_PER_SEC;

   printf("### C++ Execution time: %f\n", msec / 1000.0f);

   return 0;
}