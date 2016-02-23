/*

The MIT License (MIT)

Copyright (c) 2015 Leonardo Laguna Ruiz

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

#ifndef _VULTIN_
#define _VULTIN_
#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

static const int32_t fix_pi  = 205887;

// Type conversion
static inline float  fix_to_float(int32_t a) {
    return (float)a / 0x00010000;
}
static inline int32_t fix_from_float(float a){
    float temp = a * 0x00010000;
    return (int32_t)temp;
}
static inline int32_t fix_from_int(int a) {
    return a * 0x00010000;
}

// Basic operations for fixed point numbers
static inline int32_t fix_add(int32_t x, int32_t y){
    return x+y;
}

static inline int32_t fix_sub(int32_t x, int32_t y){
    return x-y;
}

static inline int32_t fix_mul(int32_t x, int32_t y){
    int64_t res = (int64_t) x * y;
    return res >> 16;
}

static inline int32_t fix_minus(int32_t x){
    return -x;
}

int32_t fix_div(int32_t a, int32_t b);

static inline int32_t fix_abs(int32_t x){
    return x<0?(-x):x;
}

static inline int32_t fix_min(int32_t a,int32_t b){
    return a<b?a:b;
}

static inline int32_t fix_max(int32_t a,int32_t b){
    return a>b?a:b;
}

static inline int32_t fix_clip(int32_t v,int32_t minv, int32_t maxv){
    return v>maxv?maxv:(v<minv?minv:v);
}

static inline int32_t fix_floor(int32_t x){
    return (x & 0xFFFF0000UL);
}

static inline int32_t fix_not(int32_t x){
    return ~x;
}

int32_t fix_exp(int32_t inValue);

int32_t fix_sin(int32_t inAngle);

int32_t fix_cos(int32_t inAngle);

int32_t fix_tan(int32_t inAngle);

int32_t fix_sinh(int32_t inAngle);

int32_t fix_cosh(int32_t inAngle);

int32_t fix_tanh(int32_t inAngle);


/* Floating point operations */

static inline float float_clip(float value, float low, float high){
    return value<low?low:(value>high?high:value);
}

#ifdef __cplusplus
}
#endif

#endif
