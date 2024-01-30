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
#include "vultin.h"
#include "stdio.h"

fix16_t fix_exp(fix16_t inValue) {
  if (inValue == 0)
    return 0x00010000;
  if (inValue == 0x00010000)
    return 178145;
  if (inValue >= 681391)
    return 0x7FFFFFFF;
  if (inValue <= -772243)
    return 0;
  // The power-series converges much faster on positive values
  // and exp(-x) = 1/exp(x).
  int neg = (inValue < 0);
  if (neg)
    inValue = -inValue;
  fix16_t result = inValue + 0x00010000;
  fix16_t term = inValue;
  uint_fast8_t i;
  for (i = 2; i < 30; i++) {
    term = fix_mul(term, fix_div(inValue, int_to_fix(i)));
    result += term;
    if ((term < 500) && ((i > 15) || (term < 20)))
      break;
  }
  if (neg)
    result = fix_div(0x00010000, result);
  return result;
}

fix16_t fix_sin(fix16_t x0) {
  fix16_t x1 = (x0 % 0x6487e /* 6.283185 */);
  uint8_t sign = (x1 > 0x3243f /* 3.141593 */);
  fix16_t x2 = (x1 % 0x3243f /* 3.141593 */);
  fix16_t x3;
  if (x2 > (fix16_t)0x1921f /* 1.570796 */)
    x3 = fix_add(0x3243f /* 3.141593 */, (-x2));
  else
    x3 = x2;
  fix16_t xp2 = fix_mul(x3, x3);
  fix16_t acc = fix_mul(
      x3, fix_add((fix16_t)0x10000 /* 1.000000 */,
                  fix_mul(fix_add(((fix16_t)0xffffd556 /* -0.166667 */), fix_mul((fix16_t)0x222 /* 0.008333 */, xp2)),
                          xp2)));
  return (sign ? (-acc) : acc);
}

fix16_t fix_cos(fix16_t inAngle) { return fix_sin(inAngle + (fix_pi() >> 1)); }

fix16_t fix_tan(fix16_t inAngle) { return fix_div(fix_sin(inAngle), fix_cos(inAngle)); }

fix16_t fix_sinh(fix16_t inAngle) { return fix_mul(fix_exp(inAngle) - fix_exp(-inAngle), 0x8000); }

fix16_t fix_cosh(fix16_t inAngle) { return fix_mul(fix_exp(inAngle) + fix_exp(-inAngle), 0x8000); }

fix16_t fix_tanh(fix16_t inAngle) {
  fix16_t e_x = fix_exp(inAngle);
  fix16_t m_e_x = fix_exp(-inAngle);
  return fix_div(e_x - m_e_x, e_x + m_e_x);
}

fix16_t fix_sqrt(fix16_t inValue) {
  uint8_t neg = (inValue < 0);
  uint32_t num = (uint32_t)(neg ? -inValue : inValue);
  uint32_t result = 0;
  uint32_t bit;
  uint8_t n;

  // Many numbers will be less than 15, so
  // this gives a good balance between time spent
  // in if vs. time spent in the while loop
  // when searching for the starting value.
  if (num & 0xFFF00000)
    bit = (uint32_t)1 << 30;
  else
    bit = (uint32_t)1 << 18;

  while (bit > num)
    bit >>= 2;

  // The main part is executed twice, in order to avoid
  // using 64 bit values in computations.
  for (n = 0; n < 2; n++) {
    // First we get the top 24 bits of the answer.
    while (bit) {
      if (num >= result + bit) {
        num -= result + bit;
        result = (result >> 1) + bit;
      } else {
        result = (result >> 1);
      }
      bit >>= 2;
    }

    if (n == 0) {
      // Then process it again to get the lowest 8 bits.
      if (num > 65535) {
        // The remainder 'num' is too large to be shifted left
        // by 16, so we have to add 1 to result manually and
        // adjust 'num' accordingly.
        // num = a - (result + 0.5)^2
        //   = num + result^2 - (result + 0.5)^2
        //   = num - result - 0.5
        num -= result;
        num = (num << 16) - 0x8000;
        result = (result << 16) + 0x8000;
      } else {
        num <<= 16;
        result <<= 16;
      }

      bit = 1 << 14;
    }
  }
  return (neg ? -(int32_t)result : (int32_t)result);
}

float float_random() { return (float)rand() / (float)RAND_MAX; }

fix16_t fix_random() {
  float temp = ((float)rand() / (float)RAND_MAX) * 0x00010000;
  return (fix16_t)temp;
}

int irandom() { return (int)rand(); }

void float_print(float value) { printf("%f\n", value); }
void fix_print(fix16_t value) { printf("%f\n", fix_to_float(value)); }
void int_print(int value) { printf("%i\n", value); }
void string_print(char *value) { printf("%s\n", value); }
void bool_print(uint8_t value) { printf("%s\n", value ? "true" : "false"); }

void push_byte(CustomBuffer &buffer, uint8_t byte) {
  if (!buffer.calculate_size) {
    buffer.data.push_back(byte);
  }
}

void modify_byte(CustomBuffer &buffer, int32_t index, uint8_t byte) {
  if (!buffer.calculate_size) {
    buffer.data[static_cast<uint32_t>(index)] = byte;
  }
}

uint8_t read_byte(CustomBuffer &buffer, int32_t index) {
  if (index < 0 || (size_t)index > buffer.data.size()) {
    buffer.error = true;
    return 0;
  }
  return buffer.data[static_cast<uint32_t>(index)];
}

int32_t search_field_name(CustomBuffer &buffer, CustomTypeDescr &descr, int32_t index, std::string name) {
  int32_t descr_names = get_field(buffer, descr.position, 1);                   // get the array of names
  int32_t n_elems = deserialize_int(buffer, get_field(buffer, descr_names, 0)); // get the number of elements

  // GET the first object of the array
  int32_t current_name = get_field(buffer, descr_names, 1); // get the first name
  int32_t name_position = -1;
  int32_t i = 0;

  while (i < n_elems) {
    if (match_string(buffer, current_name, name)) {
      name_position = i;
      break;
    }
    current_name = next_object(buffer, current_name);
    i++;
  }

  if (name_position < 0) {
    return -1;
  }

  return get_field(buffer, index, name_position);
}

int32_t push_header(CustomBuffer &buffer, int32_t index, uint8_t tag) {
  push_byte(buffer, tag);
  // Space for the size
  push_byte(buffer, 0);
  push_byte(buffer, 0);
  push_byte(buffer, 0);
  return index + 4;
}

int32_t push_block_header(CustomBuffer &buffer, int32_t index) { return push_header(buffer, index, BLOCK_TAG); }

int32_t push_array(CustomBuffer &buffer, int32_t index, int32_t size) {
  index = push_header(buffer, index, ARRAY_TAG); // array tag
  index = push_int(buffer, index, size);         // push the number of elements
  return index;
}

void update_size(CustomBuffer &buffer, int32_t index, int32_t size) {
  uint32_t *ptr = (uint32_t *)&size;
  uint8_t b0 = (*ptr) & 0xFF;
  uint8_t b1 = ((*ptr) >> 8) & 0xFF;
  uint8_t b2 = ((*ptr) >> 16) & 0xFF;

  modify_byte(buffer, index + 1, b0);
  modify_byte(buffer, index + 2, b1);
  modify_byte(buffer, index + 3, b2);
}

int32_t push_float(CustomBuffer &buffer, int32_t index, float value) {
  uint8_t data[4];
  float *ptr = (float *)&data;
  *ptr = value;
  push_byte(buffer, FLOAT_TAG);
  push_byte(buffer, data[0]);
  push_byte(buffer, data[1]);
  push_byte(buffer, data[2]);
  push_byte(buffer, data[3]);
  return index + 5;
}

int32_t push_int(CustomBuffer &buffer, int32_t index, int32_t value) {
  if (value <= 127 && value >= -128) {
    int8_t svalue = (int8_t)value;
    uint8_t *ptr = (uint8_t *)&svalue;
    push_byte(buffer, SMALL_INT_TAG);
    push_byte(buffer, *ptr);
    return index + 2;
  } else {
    uint8_t data[4];
    int32_t *ptr = (int32_t *)&data;
    *ptr = value;
    push_byte(buffer, INT_TAG);
    push_byte(buffer, data[0]);
    push_byte(buffer, data[1]);
    push_byte(buffer, data[2]);
    push_byte(buffer, data[3]);
    return index + 5;
  }
}

int32_t push_string(CustomBuffer &buffer, int32_t index, const std::string &str) {
  int32_t start = index;
  index = push_header(buffer, index, STRING_TAG);
  for (size_t i = 0; i < str.length(); i++) {
    push_byte(buffer, (uint8_t)str[i]);
    index++;
  }
  push_byte(buffer, 0);
  index++;
  update_size(buffer, start, index - start);
  return index;
}

// Returns the object size (the index points to the tag of the object)
int32_t block_size(CustomBuffer &buffer, int32_t index) {
  uint8_t b0 = read_byte(buffer, index + 1);
  uint8_t b1 = read_byte(buffer, index + 2);
  uint8_t b2 = read_byte(buffer, index + 3);
  return b2 << 16 | b1 << 8 | b0;
}

bool match_string(CustomBuffer &buffer, int32_t index, std::string &name) {
  if (buffer.error == true)
    return false;
  if (read_byte(buffer, index) != STRING_TAG) {
    buffer.error = true;
    return false;
  }
  // Check the size of the strings
  int32_t expected_size = static_cast<int32_t>(name.length());
  if ((block_size(buffer, index) - 5) == expected_size) {
    bool match = true;
    int32_t i = index + 4;
    int32_t j = 0;
    while (match && j < expected_size) {
      match = match && (read_byte(buffer, i) == name[static_cast<uint32_t>(j)]);
      i++;
      j++;
    }
    return match;
  } else
    return false;
}

int32_t next_object(CustomBuffer &buffer, int32_t index) {
  uint8_t tag = read_byte(buffer, (index));
  switch (tag) {
  case SMALL_INT_TAG:
    return index + 2;
  case INT_TAG:
    return index + 5;

  case FLOAT_TAG:
    return index + 5;

  default:
    uint32_t b0 = read_byte(buffer, (index + 1));
    uint32_t b1 = read_byte(buffer, (index + 2));
    uint32_t b2 = read_byte(buffer, (index + 3));
    uint32_t size = (b2 << 16 | b1 << 8 | b0);
    return index + (int32_t)size;
  }
}

int32_t first_array_element(CustomBuffer &buffer, int32_t index) {
  index = index + 4; // skip header
  return next_object(buffer, index);
}

int32_t get_field(CustomBuffer &buffer, int32_t object, int32_t field) {
  int32_t index = object + 4; // Skip the header
  int32_t n = 0;
  while (n < field) {
    index = next_object(buffer, index);
    n++;
  }
  return index;
}

CustomTypeDescr search_type_description(CustomBuffer &buffer, std::string name) {
  int32_t position = -1;

  int32_t index = 0;

  while (index < (int32_t)buffer.data.size()) {
    // Check it is a description
    if (read_byte(buffer, index) == TYPE_TAG) {
      int32_t type_name = get_field(buffer, index, 0);
      if (match_string(buffer, type_name, name)) {
        position = index;
        break;
      }
    } else
      break;
    index = next_object(buffer, index);
  }

  return CustomTypeDescr{position};
}

int32_t goto_data(CustomBuffer &buffer) {
  int32_t index = 0;
  int32_t position = -1;

  while (index < (int32_t)buffer.data.size()) {
    if (read_byte(buffer, index) == BLOCK_TAG) {
      position = index;
      break;
    }
    index = next_object(buffer, index);
  }
  return position;
}

int32_t deserialize_int(CustomBuffer &buffer, int32_t index) {
  // Check the tag
  if (read_byte(buffer, index) == SMALL_INT_TAG) {
    return read_byte(buffer, index + 1);
  } else if (read_byte(buffer, index) == INT_TAG) {
    uint8_t data[4];
    int32_t *ptr = (int32_t *)&data;
    data[0] = read_byte(buffer, index + 1);
    data[1] = read_byte(buffer, index + 2);
    data[2] = read_byte(buffer, index + 3);
    data[3] = read_byte(buffer, index + 4);
    return *ptr;
  } else {
    buffer.error = true;
    return 0;
  }
}

float deserialize_float(CustomBuffer &buffer, int32_t index) {
  // Check the tag
  if (read_byte(buffer, index) == FLOAT_TAG) {
    float *ptr = (float *)&(buffer.data[static_cast<uint32_t>(index + 1)]);
    return *ptr;
  } else {
    buffer.error = true;
    return 0.0f;
  }
}

std::string deserialize_string(CustomBuffer &buffer, int32_t index) {
  // Check the tag
  if (read_byte(buffer, index) == STRING_TAG) {
    int32_t size = block_size(buffer, index) - 5 + 1;

    index = index + 4; // move to the first characters
    std::string str;
    str.resize(static_cast<size_t>(size));
    for (int i = 0; i < size; i++) {
      str[static_cast<size_t>(i)] = (char)read_byte(buffer, index++);
    }
    return str;
  } else {
    buffer.error = true;
    return "";
  }
}
