/*

The MIT License (MIT)

Copyright (c) 2014-2024 Leonardo Laguna Ruiz

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.


NOTE: The code for the fixed-point operations is based on the project:
      https://code.google.com/p/libfixmath/

*/

#ifndef VULTIN_H
#define VULTIN_H

/* This is the core of the runtime: it depends only on the C library and
   <array>, so it does not allocate. The optional features live in their own
   headers (vultin_string.hpp, vultin_tuple.hpp, vultin_list.hpp and
   vultin_serialization.hpp), which the generated headers include only when
   their code uses them. Every generated copy of these files is identical, so
   programs sharing a directory also share them.

   The fixed-point math functions and the random number generators are
   implemented in vultin.cpp; a build can leave them out by defining
   VULT_NO_FIX16_MATH or VULT_NO_RANDOM. VULT_FULL_RUNTIME cancels both. */

#ifdef VULT_FULL_RUNTIME
#undef VULT_NO_FIX16_MATH
#undef VULT_NO_RANDOM
#endif

#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include <array>

#ifdef _MSC_VER
#define static_inline static __inline
#else
#define static_inline static inline
#endif

typedef int32_t fix16_t;

// Fixed-point constants
static const fix16_t FIX16_ONE = 0x00010000;  // 1.0 in fixed-point
static const fix16_t FIX16_MAX = 0x7FFFFFFF;  // Maximum fixed-point value
static const fix16_t FIX16_MIN = INT32_MIN;     // Minimum fixed-point value

extern float float_samplerate();
extern fix16_t fix_samplerate();

// Type conversion
static_inline float fix_to_float(fix16_t a) { return (float)a / FIX16_ONE; }
static_inline bool fix_to_bool(fix16_t a) { return a != 0; }
static_inline fix16_t float_to_fix(float a) {
  float temp = a * FIX16_ONE;
  if (temp >= (float)FIX16_MAX) return FIX16_MAX;
  if (temp <= (float)FIX16_MIN) return FIX16_MIN;
  return (fix16_t)temp;
}
static_inline bool float_to_bool(float a) { return a != 0.0f; }

static_inline float int_to_float(int a) { return (float)a; }

static_inline bool int_to_bool(int a) { return a != 0; }

static_inline float bool_to_float(bool a) { return a ? 1.0 : 0.0; }

static_inline fix16_t bool_to_fix(bool a) { return a ? float_to_fix(1.0) : float_to_fix(0.0); }

static_inline int float_to_int(float a) { return (int)a; }

static_inline fix16_t int_to_fix(int a) {
  if (a > 32767) return FIX16_MAX;
  if (a < -32768) return FIX16_MIN;
  return a * FIX16_ONE;
}

static_inline fix16_t fix_to_fix(fix16_t a) { return a; }

static_inline int fix_to_int(fix16_t a) { return (a >> 16); }

static_inline int int_clip(int v, int minv, int maxv) { return v > maxv ? maxv : (v < minv ? minv : v); }

// Int16 type conversion functions
static_inline int16_t int_to_int16(int a) { return (int16_t)(a < -32768 ? -32768 : (a > 32767 ? 32767 : a)); }
static_inline int16_t float_to_int16(float a) { return int_to_int16((int)a); }
static_inline int16_t bool_to_int16(bool a) { return a ? 1 : 0; }
static_inline int16_t fix_to_int16(fix16_t a) { return int_to_int16(fix_to_int(a)); }
static_inline int16_t int16_to_int16(int16_t a) { return a; }

static_inline int int16_to_int(int16_t a) { return (int)a; }
static_inline float int16_to_float(int16_t a) { return (float)a; }
static_inline bool int16_to_bool(int16_t a) { return a != 0; }
static_inline fix16_t int16_to_fix(int16_t a) { return int_to_fix((int)a); }

// Int16 arithmetic operations with clamping
static_inline int16_t int16_add(int16_t a, int16_t b) { return int_to_int16((int)a + (int)b); }
static_inline int16_t int16_sub(int16_t a, int16_t b) { return int_to_int16((int)a - (int)b); }
static_inline int16_t int16_mul(int16_t a, int16_t b) { return int_to_int16((int)a * (int)b); }
static_inline int16_t int16_div(int16_t a, int16_t b) { return b == 0 ? 0 : int_to_int16((int)a / (int)b); }
static_inline int16_t int16_mod(int16_t a, int16_t b) { return b == 0 ? 0 : (int16_t)((int)a % (int)b); }

static_inline int16_t int16_abs(int16_t a) { return a == -32768 ? 32767 : (a < 0 ? (int16_t)(-a) : a); }
static_inline int16_t int16_min(int16_t a, int16_t b) { return a < b ? a : b; }
static_inline int16_t int16_max(int16_t a, int16_t b) { return a > b ? a : b; }

// Basic operations for fixed point numbers
static_inline fix16_t fix_add(fix16_t x, fix16_t y) { return x + y; }

static_inline fix16_t fix_sub(fix16_t x, fix16_t y) { return x - y; }

static_inline fix16_t fix_mul(fix16_t x, fix16_t y) {
  int64_t res = (int64_t)x * y;
  return (fix16_t)(res >> 16);
}

static_inline fix16_t fix_div(fix16_t a, fix16_t b) {
  if (b == 0)
    return 0;
  // The whole division has to happen in 64 bits. Narrowing the shifted numerator first
  // truncates it, which returns 0 for any |a| >= 0.5.
  int64_t res = (((int64_t)a) << 16) / (int64_t)b;
  return (fix16_t)res;
}

static_inline fix16_t fix_abs(fix16_t x) {
  if (x == FIX16_MIN) return FIX16_MAX;
  return x < 0 ? -x : x;
}

static_inline fix16_t fix_clip(fix16_t v, fix16_t minv, fix16_t maxv) {
  return v > maxv ? maxv : (v < minv ? minv : v);
}

static_inline fix16_t fix_floor(fix16_t x) { return (x & (fix16_t)0xFFFF0000); }

static_inline fix16_t fix_ceil(fix16_t x) { return (x & (fix16_t)0xFFFF) ? ((x & (fix16_t)0xFFFF0000) + 0x10000) : x; }

static_inline fix16_t fix_min(fix16_t a, fix16_t b) { return a < b ? a : b; }

static_inline fix16_t fix_max(fix16_t a, fix16_t b) { return a > b ? a : b; }

static_inline int int_min(int a, int b) { return a < b ? a : b; }

static_inline int int_max(int a, int b) { return a > b ? a : b; }

static_inline float float_eps() { return 1e-18f; }

static_inline fix16_t fix_eps() { return 1; }

static_inline float float_pi() { return 3.1415926535897932384f; }

static_inline fix16_t fix_pi() { return 205887; }

#ifndef VULT_NO_FIX16_MATH
fix16_t fix_exp(fix16_t inValue);

fix16_t fix_sin(fix16_t inAngle);

fix16_t fix_cos(fix16_t inAngle);

fix16_t fix_tan(fix16_t inAngle);

fix16_t fix_sinh(fix16_t inAngle);

fix16_t fix_cosh(fix16_t inAngle);

fix16_t fix_tanh(fix16_t inAngle);

fix16_t fix_sqrt(fix16_t inValue);
#endif // VULT_NO_FIX16_MATH

/* Floating point operations */

static_inline float float_clip(float value, float low, float high) {
  return value < low ? low : (value > high ? high : value);
}

static_inline float float_min(float a, float b) { return a < b ? a : b; }

static_inline float float_max(float a, float b) { return a > b ? a : b; }

#ifndef VULT_NO_RANDOM
/* Random numbers */
float float_random();
fix16_t fix_random();
int int_random();
#endif // VULT_NO_RANDOM

#endif // VULTIN_H
