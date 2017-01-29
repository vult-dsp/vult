

#include "bench.h"
#include "vultin.h"

int main(void)
{
   Bench_process_type data;
   Bench_process_init(data);

   int time = 44100 * 100;

   while (time > 0)
   {
      Bench_process(data);
      time--;
   }

   return 0;
}