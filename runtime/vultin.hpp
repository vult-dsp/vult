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

#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include <array>
#include <tuple>
#include <string>
#include <vector>

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

static_inline std::string fix_to_string(fix16_t a) { return std::to_string(fix_to_float(a)); }

static_inline std::string bool_to_string(bool a) {
  if (a)
    return std::string("true");
  else
    return std::string("false");
}

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
static_inline std::string int16_to_string(int16_t a) { return std::to_string(a); }

// Int16 arithmetic operations with clamping
static_inline int16_t int16_add(int16_t a, int16_t b) { return int_to_int16((int)a + (int)b); }
static_inline int16_t int16_sub(int16_t a, int16_t b) { return int_to_int16((int)a - (int)b); }
static_inline int16_t int16_mul(int16_t a, int16_t b) { return int_to_int16((int)a * (int)b); }
static_inline int16_t int16_div(int16_t a, int16_t b) { return b == 0 ? 0 : int_to_int16((int)a / (int)b); }
static_inline int16_t int16_mod(int16_t a, int16_t b) { return b == 0 ? 0 : (int16_t)((int)a % (int)b); }

static_inline int16_t int16_abs(int16_t a) { return a == -32768 ? 32767 : (a < 0 ? -a : a); }
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
  fix16_t result = (fix16_t)(((int64_t)a) << 16) / ((int64_t)b);
  return result;
}

static_inline fix16_t fix_abs(fix16_t x) {
  if (x == FIX16_MIN) return FIX16_MAX;
  return x < 0 ? -x : x;
}

static_inline fix16_t fix_clip(fix16_t v, fix16_t minv, fix16_t maxv) {
  return v > maxv ? maxv : (v < minv ? minv : v);
}

static_inline fix16_t fix_floor(fix16_t x) { return (x & (fix16_t)0xFFFF0000); }

static_inline float float_eps() { return 1e-18f; }

static_inline fix16_t fix_eps() { return 1; }

static_inline float float_pi() { return 3.1415926535897932384f; }

static_inline fix16_t fix_pi() { return 205887; }

fix16_t fix_exp(fix16_t inValue);

fix16_t fix_sin(fix16_t inAngle);

fix16_t fix_cos(fix16_t inAngle);

fix16_t fix_tan(fix16_t inAngle);

fix16_t fix_sinh(fix16_t inAngle);

fix16_t fix_cosh(fix16_t inAngle);

fix16_t fix_tanh(fix16_t inAngle);

fix16_t fix_sqrt(fix16_t inValue);

/* Floating point operations */

static_inline float float_clip(float value, float low, float high) {
  return value < low ? low : (value > high ? high : value);
}

/* Random numbers */
float float_random();
fix16_t fix_random();
int int_random();

/* Serialization */

static const uint8_t TYPE_TAG = 't';
static const uint8_t BLOCK_TAG = 'b';
static const uint8_t STRING_TAG = 's';
static const uint8_t FLOAT_TAG = 'f';
static const uint8_t SMALL_INT_TAG = 'i';
static const uint8_t INT_TAG = 'I';
static const uint8_t ARRAY_TAG = 'a';

typedef struct CustomBuffer {
  std::vector<uint8_t> data;
  bool calculate_size;
  bool error;
} CustomBuffer;

typedef struct CustomTypeDescr {
  int32_t position;
} CustomTypeDescr;

// Safe type conversion unions to avoid type punning
union float_bytes_union {
  float f;
  uint8_t bytes[4];
};

union int32_bytes_union {
  int32_t i;
  uint8_t bytes[4];
};

union uint32_bytes_union {
  uint32_t u;
  uint8_t bytes[4];
};

union int8_bytes_union {
  int8_t i;
  uint8_t byte;
};

int32_t search_field_name(CustomBuffer &buffer, CustomTypeDescr &descr, int32_t index, std::string name);

void update_size(CustomBuffer &buffer, int32_t index, int32_t size);

int32_t push_block_header(CustomBuffer &buffer, int32_t index);

int32_t push_header(CustomBuffer &buffer, int32_t index, uint8_t tag);

int32_t push_float(CustomBuffer &buffer, int32_t index, float value);

int32_t push_int(CustomBuffer &buffer, int32_t index, int32_t value);

int32_t push_string(CustomBuffer &buffer, int32_t index, const std::string &str);

int32_t push_array(CustomBuffer &buffer, int32_t index, int32_t size);

int32_t get_field(CustomBuffer &buffer, int32_t object, int32_t field);

bool match_string(CustomBuffer &buffer, int32_t index, std::string &name);

int32_t next_object(CustomBuffer &buffer, int32_t index);

template <std::size_t SIZE>
int32_t serialize_type_descr(CustomBuffer &buffer, int32_t index, const std::string &str,
                             std::array<std::string, SIZE> &fields) {
  int32_t start = index;
  index = push_header(buffer, index, TYPE_TAG);
  index = push_string(buffer, index, str);
  int32_t array_start = index;
  index = push_array(buffer, index, (int32_t)fields.size());
  for (size_t i = 0; i < fields.size(); i++) {
    index = push_string(buffer, index, fields[i]);
  }
  update_size(buffer, array_start, index - array_start);
  update_size(buffer, start, index - start);
  return index;
}

CustomTypeDescr search_type_description(CustomBuffer &buffer, std::string name);

int32_t deserialize_int(CustomBuffer &buffer, int32_t index);

float deserialize_float(CustomBuffer &buffer, int32_t index);

std::string deserialize_string(CustomBuffer &buffer, int32_t index);

int32_t goto_data(CustomBuffer &buffer);

int32_t first_array_element(CustomBuffer &buffer, int32_t index);

int32_t get_array_count(CustomBuffer &buffer, int32_t index);

template <std::size_t SIZE, typename DATA>
void serialize_data(CustomBuffer &buffer,
                    int32_t (*serialize_type_descr_function)(CustomBuffer &, int32_t, std::array<bool, SIZE> &),
                    int32_t (*serialize_data_function)(CustomBuffer &, int32_t, const DATA &), const DATA &data) {
  buffer.data.resize(0);
  // First we run the serializer without actually attaching data.
  // This is done to calculate the size of buffer needed.
  buffer.calculate_size = true;
  // This is an array that where we mark if a type description has been serialized
  std::array<bool, SIZE> marks;
  // initially nothing has been serialized
  marks.fill(false);
  // The serializers are ran in order to calculate the size.
  int32_t index = serialize_type_descr_function(buffer, 0, marks);
  index = serialize_data_function(buffer, index, data);
  // TODO: calculate the checksum
  // Once we know the size, the buffer is allocated
  buffer.data.reserve((size_t)index);
  // next we are going to actually write data to the buffer
  buffer.calculate_size = false;
  // all marks are reset to false
  marks.fill(false);
  // Finally the data is written to the buffer.
  index = serialize_type_descr_function(buffer, 0, marks);
  index = serialize_data_function(buffer, index, data);
}

template <typename DATA>
void deserialize_data(CustomBuffer &buffer, void (*deserializer)(CustomBuffer &, CustomTypeDescr &, int32_t, DATA &),
                      std::string type_name, DATA &data) {

  buffer.error = false;
  buffer.calculate_size = false;
  // search the information about the type
  CustomTypeDescr descr = search_type_description(buffer, type_name);

  if (descr.position < 0) {
    buffer.error = true;
    return;
  }
  // skip the type descriptions
  int32_t data_index = goto_data(buffer);
  // call the deserializer
  deserializer(buffer, descr, data_index, data);
}

#endif // VULTIN_H
