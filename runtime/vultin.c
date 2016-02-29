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
#include "vultin.h"

#ifdef __GNUC__
#define clz(x) (__builtin_clzl(x) - (8 * sizeof(long) - 32))
#else
static uint8_t clz(uint32_t x)
{
    uint8_t result = 0;
    if (x == 0) return 32;
    while (!(x & 0xF0000000)) { result += 4; x <<= 4; }
    while (!(x & 0x80000000)) { result += 1; x <<= 1; }
    return result;
}
#endif

int32_t fix_div(int32_t a, int32_t b)
{
    if (b == 0) return 0;
    uint32_t remainder = (a >= 0) ? a : (-a);
    uint32_t divider = (b >= 0) ? b : (-b);
    uint32_t quotient = 0;
    int bit_pos = 17;
    if (divider & 0xFFF00000) {
        uint32_t shifted_div = ((divider >> 17) + 1);
        quotient = remainder / shifted_div;
        remainder -= ((uint64_t)quotient * divider) >> 17;
    }
    while (!(divider & 0xF) && bit_pos >= 4) {
        divider >>= 4;
        bit_pos -= 4;
    }
    while (remainder && bit_pos >= 0) {
        int shift = clz(remainder);
        if (shift > bit_pos) shift = bit_pos;
        remainder <<= shift;
        bit_pos -= shift;
        uint32_t div = remainder / divider;
        remainder = remainder % divider;
        quotient += div << bit_pos;
        remainder <<= 1;
        bit_pos--;
    }
    int32_t result = quotient >> 1;
    if ((a ^ b) & 0x80000000) {
        result = -result;
    }
    return result;
}

int32_t fix_exp(int32_t inValue) {
    if(inValue == 0        ) return 0x00010000;
    if(inValue == 0x00010000) return 178145;
    if(inValue >= 681391   ) return 0x7FFFFFFF;
    if(inValue <= -772243  ) return 0;
    // The power-series converges much faster on positive values
    // and exp(-x) = 1/exp(x).
    int neg = (inValue < 0);
    if (neg) inValue = -inValue;
    int32_t result = inValue + 0x00010000;
    int32_t term = inValue;
    uint_fast8_t i;
    for (i = 2; i < 30; i++) {
        term = fix_mul(term, fix_div(inValue, int_to_fix(i)));
        result += term;
        if ((term < 500) && ((i > 15) || (term < 20))) break;
    }
    if (neg) result = fix_div(0x00010000, result);
    return result;
}

int32_t fix_sin(int32_t inAngle)
{
    int32_t tempAngle = inAngle % (fix_pi << 1);

    if(tempAngle > fix_pi)
        tempAngle -= (fix_pi << 1);
    else if(tempAngle < -fix_pi)
        tempAngle += (fix_pi << 1);

    int32_t tempAngleSq = fix_mul(tempAngle, tempAngle);

    int32_t tempOut;
    tempOut = fix_mul(-13, tempAngleSq) + 546;
    tempOut = fix_mul(tempOut, tempAngleSq) - 10923;
    tempOut = fix_mul(tempOut, tempAngleSq) + 65536;
    tempOut = fix_mul(tempOut, tempAngle);

    return tempOut;
}

int32_t fix_cos(int32_t inAngle)
{
    return fix_sin(inAngle + (fix_pi >> 1));
}

int32_t fix_tan(int32_t inAngle)
{
    return fix_div(fix_sin(inAngle), fix_cos(inAngle));
}

int32_t fix_sinh(int32_t inAngle)
{
    return fix_mul(fix_exp(inAngle)-fix_exp(-inAngle),0x8000);
}

int32_t fix_cosh(int32_t inAngle)
{
    return fix_mul(fix_exp(inAngle)+fix_exp(-inAngle),0x8000);
}

int32_t fix_tanh(int32_t inAngle)
{
    int32_t e_x = fix_exp(inAngle);
    int32_t m_e_x = fix_exp(-inAngle);
    return fix_div(e_x-m_e_x,e_x+m_e_x);
}

int32_t fix_sqrt(int32_t inValue)
{
    uint8_t  neg = (inValue < 0);
    uint32_t num = (neg ? -inValue : inValue);
    uint32_t result = 0;
    uint32_t bit;
    uint8_t  n;

    // Many numbers will be less than 15, so
    // this gives a good balance between time spent
    // in if vs. time spent in the while loop
    // when searching for the starting value.
    if (num & 0xFFF00000)
        bit = (uint32_t)1 << 30;
    else
        bit = (uint32_t)1 << 18;

    while (bit > num) bit >>= 2;

    // The main part is executed twice, in order to avoid
    // using 64 bit values in computations.
    for (n = 0; n < 2; n++)
    {
        // First we get the top 24 bits of the answer.
        while (bit)
        {
            if (num >= result + bit)
            {
                num -= result + bit;
                result = (result >> 1) + bit;
            }
            else
            {
                result = (result >> 1);
            }
            bit >>= 2;
        }

        if (n == 0)
        {
            // Then process it again to get the lowest 8 bits.
            if (num > 65535)
            {
                // The remainder 'num' is too large to be shifted left
                // by 16, so we have to add 1 to result manually and
                // adjust 'num' accordingly.
                // num = a - (result + 0.5)^2
                //   = num + result^2 - (result + 0.5)^2
                //   = num - result - 0.5
                num -= result;
                num = (num << 16) - 0x8000;
                result = (result << 16) + 0x8000;
            }
            else
            {
                num <<= 16;
                result <<= 16;
            }

            bit = 1 << 14;
        }
    }
    return (neg ? -(int32_t)result : (int32_t)result);
}

/* Array initialization */
void float_init_array(float *data,int size, float value) {
    int i;
    for(i=0;i<size;i++)
        data[i] = value;
}

void int_init_array  (int *data,int size, int value) {
    int i;
    for(i=0;i<size;i++)
        data[i] = value;
}

void bool_init_array (int8_t *data,int size, int8_t value) {
    int i;
    for(i=0;i<size;i++)
        data[i] = value;
}

void fix_init_array  (int32_t *data,int size, int32_t value) {
    int i;
    for(i=0;i<size;i++)
        data[i] = value;
}
