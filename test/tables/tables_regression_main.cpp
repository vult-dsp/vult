/* Functional checks for the code generated from tables_regression.vult. Each check evaluates a
   generated table at the edges and compares it against the function the table was fitted on.
   The test runner builds this file together with the generated code and asserts that it runs
   successfully. */
#include "tables_regression.h"
#include <cmath>
#include <cstdio>

static int failures = 0;

static void check(const char* name, float got, float expected, float tolerance) {
   if (std::fabs(got - expected) > tolerance) {
      std::printf("FAIL %s: got %.6f, expected %.6f\n", name, got, expected);
      failures++;
   }
}

int main() {
   /* The exact upper endpoint of a table without bound checks must return the value at max
      instead of reading past the fitted cells. */
   check("order1_nocheck(max)", Tables_regression_order1_nocheck(1.0f), std::tanh(2.0f), 1e-4f);
   check("order2_nocheck(max)", Tables_regression_order2_nocheck(1.0f), std::tanh(2.0f), 1e-4f);
   check("order1_nocheck(min)", Tables_regression_order1_nocheck(0.0f), 0.0f, 1e-4f);
   check("order2_nocheck(min)", Tables_regression_order2_nocheck(0.0f), 0.0f, 1e-4f);
   check("order1_nocheck(mid)", Tables_regression_order1_nocheck(0.5f), std::tanh(1.0f), 1e-2f);
   check("order2_nocheck(mid)", Tables_regression_order2_nocheck(0.5f), std::tanh(1.0f), 1e-3f);

   /* A bounds-checked table with a fix16 input and a real output must clamp inputs far outside
      the range instead of letting the index scaling wrap in fixed point. */
   check("mixed_checked(10000)", Tables_regression_mixed_checked(float_to_fix(10000.0f)), 1.0f, 1e-3f);
   check("mixed_checked(-10000)", Tables_regression_mixed_checked(float_to_fix(-10000.0f)), 0.0f, 1e-3f);
   check("mixed_checked(max)", Tables_regression_mixed_checked(float_to_fix(1.0f)), 1.0f, 1e-3f);
   check("mixed_checked(mid)", Tables_regression_mixed_checked(float_to_fix(0.5f)), 0.5f, 1e-3f);

   /* The n samples of a wavetable span n - 1 cells: both endpoints and the interior grid
      points must land exactly on the samples. */
   int n = Tables_regression_wavetable_samples();
   check("wavetable(0)", Tables_regression_wavetable(0.0f), Tables_regression_wave_raw(0, 0), 1e-4f);
   check("wavetable(1)", Tables_regression_wavetable(1.0f), Tables_regression_wave_raw(0, n - 1), 1e-4f);
   const int grid_points[] = {1, n / 2, n - 2};
   for (int i = 0; i < 3; i++) {
      int p = grid_points[i];
      float x = (float)p / (float)(n - 1);
      char name[64];
      std::snprintf(name, sizeof(name), "wavetable(%d/(n-1))", p);
      check(name, Tables_regression_wavetable(x), Tables_regression_wave_raw(0, p), 1e-4f);
   }

   if (failures != 0) {
      return 1;
   }
   std::printf("all table regression checks passed\n");
   return 0;
}
