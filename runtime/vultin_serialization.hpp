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


*/

#ifndef VULTIN_SERIALIZATION_H
#define VULTIN_SERIALIZATION_H

/* Serialization of the Vult runtime. Header-only, so that only the programs
   that save their state compile it. Included by the generated headers whose
   code uses it. */

#include "vultin.hpp"
#include "vultin_string.hpp"
#include <vector>

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

inline int32_t search_field_name(CustomBuffer &buffer, CustomTypeDescr &descr, int32_t index, std::string name);

inline void update_size(CustomBuffer &buffer, int32_t index, int32_t size);

inline int32_t push_block_header(CustomBuffer &buffer, int32_t index);

inline int32_t push_header(CustomBuffer &buffer, int32_t index, uint8_t tag);

inline int32_t push_float(CustomBuffer &buffer, int32_t index, float value);

inline int32_t push_int(CustomBuffer &buffer, int32_t index, int32_t value);

inline int32_t push_string(CustomBuffer &buffer, int32_t index, const std::string &str);

inline int32_t push_array(CustomBuffer &buffer, int32_t index, int32_t size);

inline int32_t get_field(CustomBuffer &buffer, int32_t object, int32_t field);

inline bool match_string(CustomBuffer &buffer, int32_t index, std::string &name);

inline int32_t next_object(CustomBuffer &buffer, int32_t index);

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

inline CustomTypeDescr search_type_description(CustomBuffer &buffer, std::string name);

inline int32_t deserialize_int(CustomBuffer &buffer, int32_t index);

inline float deserialize_float(CustomBuffer &buffer, int32_t index);

inline std::string deserialize_string(CustomBuffer &buffer, int32_t index);

inline int32_t goto_data(CustomBuffer &buffer);

inline int32_t first_array_element(CustomBuffer &buffer, int32_t index);

inline int32_t get_array_count(CustomBuffer &buffer, int32_t index);

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

inline void push_byte(CustomBuffer &buffer, uint8_t byte) {
  if (!buffer.calculate_size) {
    buffer.data.push_back(byte);
  }
}

inline void modify_byte(CustomBuffer &buffer, int32_t index, uint8_t byte) {
  if (!buffer.calculate_size) {
    buffer.data[static_cast<uint32_t>(index)] = byte;
  }
}

inline uint8_t read_byte(CustomBuffer &buffer, int32_t index) {
  if (index < 0 || (size_t)index >= buffer.data.size()) {
    buffer.error = true;
    return 0;
  }
  return buffer.data[static_cast<uint32_t>(index)];
}

inline int32_t search_field_name(CustomBuffer &buffer, CustomTypeDescr &descr, int32_t index, std::string name) {
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

inline int32_t push_header(CustomBuffer &buffer, int32_t index, uint8_t tag) {
  push_byte(buffer, tag);
  // Space for the size
  push_byte(buffer, 0);
  push_byte(buffer, 0);
  push_byte(buffer, 0);
  return index + 4;
}

inline int32_t push_block_header(CustomBuffer &buffer, int32_t index) { return push_header(buffer, index, BLOCK_TAG); }

inline int32_t push_array(CustomBuffer &buffer, int32_t index, int32_t size) {
  index = push_header(buffer, index, ARRAY_TAG); // array tag
  index = push_int(buffer, index, size);         // push the number of elements
  return index;
}

inline void update_size(CustomBuffer &buffer, int32_t index, int32_t size) {
  uint32_bytes_union converter;
  converter.u = static_cast<uint32_t>(size);

  modify_byte(buffer, index + 1, converter.bytes[0]);
  modify_byte(buffer, index + 2, converter.bytes[1]);
  modify_byte(buffer, index + 3, converter.bytes[2]);
}

inline int32_t push_float(CustomBuffer &buffer, int32_t index, float value) {
  float_bytes_union converter;
  converter.f = value;

  push_byte(buffer, FLOAT_TAG);
  push_byte(buffer, converter.bytes[0]);
  push_byte(buffer, converter.bytes[1]);
  push_byte(buffer, converter.bytes[2]);
  push_byte(buffer, converter.bytes[3]);
  return index + 5;
}

inline int32_t push_int(CustomBuffer &buffer, int32_t index, int32_t value) {
  if (value <= 127 && value >= -128) {
    int8_bytes_union converter;
    converter.i = static_cast<int8_t>(value);
    push_byte(buffer, SMALL_INT_TAG);
    push_byte(buffer, converter.byte);
    return index + 2;
  } else {
    int32_bytes_union converter;
    converter.i = value;
    push_byte(buffer, INT_TAG);
    push_byte(buffer, converter.bytes[0]);
    push_byte(buffer, converter.bytes[1]);
    push_byte(buffer, converter.bytes[2]);
    push_byte(buffer, converter.bytes[3]);
    return index + 5;
  }
}

inline int32_t push_string(CustomBuffer &buffer, int32_t index, const std::string &str) {
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
inline int32_t block_size(CustomBuffer &buffer, int32_t index) {
  uint8_t b0 = read_byte(buffer, index + 1);
  uint8_t b1 = read_byte(buffer, index + 2);
  uint8_t b2 = read_byte(buffer, index + 3);
  return b2 << 16 | b1 << 8 | b0;
}

inline bool match_string(CustomBuffer &buffer, int32_t index, std::string &name) {
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

inline int32_t next_object(CustomBuffer &buffer, int32_t index) {
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

inline int32_t first_array_element(CustomBuffer &buffer, int32_t index) {
  index = index + 4; // skip header
  return next_object(buffer, index);
}

inline int32_t get_array_count(CustomBuffer &buffer, int32_t index) {
  index = index + 4; // skip header (tag + size bytes)
  return deserialize_int(buffer, index);
}

inline int32_t get_field(CustomBuffer &buffer, int32_t object, int32_t field) {
  int32_t index = object + 4; // Skip the header
  int32_t n = 0;
  while (n < field) {
    index = next_object(buffer, index);
    n++;
  }
  return index;
}

inline CustomTypeDescr search_type_description(CustomBuffer &buffer, std::string name) {
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
    int32_t prev_index = index;
    index = next_object(buffer, index);
    if (index <= prev_index) { buffer.error = true; break; }
  }

  return CustomTypeDescr{position};
}

inline int32_t goto_data(CustomBuffer &buffer) {
  int32_t index = 0;
  int32_t position = -1;

  while (index < (int32_t)buffer.data.size()) {
    if (read_byte(buffer, index) == BLOCK_TAG) {
      position = index;
      break;
    }
    int32_t prev_index = index;
    index = next_object(buffer, index);
    if (index <= prev_index) { buffer.error = true; break; }
  }
  return position;
}

inline int32_t deserialize_int(CustomBuffer &buffer, int32_t index) {
  // Check the tag
  if (read_byte(buffer, index) == SMALL_INT_TAG) {
    int8_bytes_union converter;
    converter.byte = read_byte(buffer, index + 1);
    return static_cast<int32_t>(converter.i);
  } else if (read_byte(buffer, index) == INT_TAG) {
    int32_bytes_union converter;
    converter.bytes[0] = read_byte(buffer, index + 1);
    converter.bytes[1] = read_byte(buffer, index + 2);
    converter.bytes[2] = read_byte(buffer, index + 3);
    converter.bytes[3] = read_byte(buffer, index + 4);
    return converter.i;
  } else {
    buffer.error = true;
    return 0;
  }
}

inline float deserialize_float(CustomBuffer &buffer, int32_t index) {
  // Check the tag
  if (read_byte(buffer, index) == FLOAT_TAG) {
    float_bytes_union converter;
    converter.bytes[0] = read_byte(buffer, index + 1);
    converter.bytes[1] = read_byte(buffer, index + 2);
    converter.bytes[2] = read_byte(buffer, index + 3);
    converter.bytes[3] = read_byte(buffer, index + 4);
    return converter.f;
  } else {
    buffer.error = true;
    return 0.0f;
  }
}

inline std::string deserialize_string(CustomBuffer &buffer, int32_t index) {
  // Check the tag
  if (read_byte(buffer, index) == STRING_TAG) {
    int32_t total_size = block_size(buffer, index);
    // total_size = header(4) + string_content + null_terminator(1)
    // string_length = total_size - 4 - 1 = total_size - 5
    int32_t string_length = total_size - 5;

    if (string_length < 0) {
      buffer.error = true;
      return "";
    }

    index = index + 4; // move to the first characters
    std::string str;
    str.reserve(static_cast<size_t>(string_length));

    for (int i = 0; i < string_length; i++) {
      char c = static_cast<char>(read_byte(buffer, index++));
      if (buffer.error) {
        return "";
      }
      str.push_back(c);
    }

    // Skip the null terminator
    index++;

    return str;
  } else {
    buffer.error = true;
    return "";
  }
}

#endif // VULTIN_SERIALIZATION_H
