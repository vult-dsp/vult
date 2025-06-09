/*
  Vult JSON Serializer Implementation (Jansson)

  The MIT License (MIT)
  Copyright (c) 2014-2024 Leonardo Laguna Ruiz

  This file implements the VultJsonSerializerJansson class for converting between
  Vult's binary serialization format and JSON using the Jansson C library.
*/

#include "json_serializer_jansson.hpp"
#include <sstream>
#include <cstring>

void VultJsonSerializerJansson::parseTypeDescriptions(CustomBuffer &buffer) {
  type_registry.clear();
  int32_t index = 0;

  while (index < static_cast<int32_t>(buffer.data.size())) {
    uint8_t tag = read_byte(buffer, index);

    if (tag == TYPE_TAG) {
      // Parse type description
      int32_t type_start = index;
      int32_t type_size = block_size(buffer, index);
      index += 4; // Skip header

      // Read type name (first string)
      std::string type_name = deserialize_string(buffer, index);
      index = next_object(buffer, index);

      TypeInfo info;
      info.name = type_name;

      // Read field names
      while (index < type_start + type_size) {
        if (read_byte(buffer, index) == STRING_TAG) {
          std::string field_name = deserialize_string(buffer, index);
          info.fields.push_back(field_name);
        }
        index = next_object(buffer, index);
      }

      type_registry[type_name] = info;
    } else if (tag == BLOCK_TAG) {
      // Found data section, stop parsing types
      break;
    } else {
      index = next_object(buffer, index);
    }
  }
}

json_t *VultJsonSerializerJansson::parseDataValue(CustomBuffer &buffer, int32_t &index, const std::string &expected_type) {
  uint8_t tag = read_byte(buffer, index);

  switch (tag) {
  case SMALL_INT_TAG:
  case INT_TAG: {
    int32_t value = deserialize_int(buffer, index);
    index = next_object(buffer, index);
    return json_integer(value);
  }
  case FLOAT_TAG: {
    float value = deserialize_float(buffer, index);
    index = next_object(buffer, index);
    return json_real(value);
  }
  case STRING_TAG: {
    std::string value = deserialize_string(buffer, index);
    index = next_object(buffer, index);
    return json_string(value.c_str());
  }
  case BLOCK_TAG: {
    json_t *obj = json_object();
    int32_t block_start = index;
    int32_t block_size_val = block_size(buffer, index);
    index += 4; // Skip header

    // Try to find type info for this block
    if (!expected_type.empty() && type_registry.find(expected_type) != type_registry.end()) {
      const TypeInfo &type_info = type_registry[expected_type];

      // Parse fields according to type description
      for (const std::string &field_name : type_info.fields) {
        if (index >= block_start + block_size_val)
          break;

        json_t *field_value = parseDataValue(buffer, index);
        if (field_value) {
          json_object_set_new(obj, field_name.c_str(), field_value);
        }
      }
    } else {
      // Unknown type, parse as generic object with numbered fields
      int field_count = 0;
      while (index < block_start + block_size_val) {
        json_t *field_value = parseDataValue(buffer, index);
        if (field_value) {
          std::string field_name = "field_" + std::to_string(field_count++);
          json_object_set_new(obj, field_name.c_str(), field_value);
        }
      }
    }

    return obj;
  }
  case ARRAY_TAG: {
    json_t *arr = json_array();
    int32_t array_start = index;
    int32_t array_size_val = block_size(buffer, index);
    index += 4; // Skip header

    // Read number of elements
    int32_t num_elements = deserialize_int(buffer, index);
    index = next_object(buffer, index);

    // Parse each element
    for (int32_t i = 0; i < num_elements && index < array_start + array_size_val; i++) {
      json_t *element = parseDataValue(buffer, index);
      if (element) {
        json_array_append_new(arr, element);
      }
    }

    return arr;
  }
  default:
    // Unknown tag, skip
    index = next_object(buffer, index);
    return nullptr;
  }
}

int32_t VultJsonSerializerJansson::writeDataValue(CustomBuffer &buffer, int32_t index, json_t *value) {
  if (!value || json_is_null(value)) {
    // Handle null - write as zero int
    return push_int(buffer, index, 0);
  } else if (json_is_boolean(value)) {
    // Convert boolean to int (0 or 1)
    return push_int(buffer, index, json_is_true(value) ? 1 : 0);
  } else if (json_is_integer(value)) {
    // Integer value
    return push_int(buffer, index, static_cast<int32_t>(json_integer_value(value)));
  } else if (json_is_real(value)) {
    // Float value
    return push_float(buffer, index, static_cast<float>(json_real_value(value)));
  } else if (json_is_string(value)) {
    // String value
    return push_string(buffer, index, json_string_value(value));
  } else if (json_is_array(value)) {
    // Array value
    int32_t start = index;
    size_t array_size = json_array_size(value);
    index = push_array(buffer, index, static_cast<int32_t>(array_size));

    for (size_t i = 0; i < array_size; i++) {
      json_t *element = json_array_get(value, i);
      index = writeDataValue(buffer, index, element);
    }

    update_size(buffer, start, index - start);
    return index;
  } else if (json_is_object(value)) {
    // Object/struct value
    int32_t start = index;
    index = push_block_header(buffer, index);

    // Iterate through object keys
    const char *key;
    json_t *val;
    json_object_foreach(value, key, val) {
      index = writeDataValue(buffer, index, val);
    }

    update_size(buffer, start, index - start);
    return index;
  }

  return index;
}

int32_t VultJsonSerializerJansson::writeTypeDescriptions(CustomBuffer &buffer, int32_t index) {
  for (const auto &[type_name, type_info] : type_registry) {
    index = serialize_type_descr(buffer, index, type_name, type_info.fields);
  }
  return index;
}

int32_t VultJsonSerializerJansson::serialize_type_descr(CustomBuffer &buffer, int32_t index,
                                                        const std::string &type_name,
                                                        const std::vector<std::string> &field_names) {
  int32_t start = index;
  index = push_header(buffer, index, TYPE_TAG);
  index = push_string(buffer, index, type_name);

  for (const std::string &field_name : field_names) {
    index = push_string(buffer, index, field_name);
  }

  update_size(buffer, start, index - start);
  return index;
}

int32_t VultJsonSerializerJansson::push_header(CustomBuffer &buffer, int32_t index, uint8_t tag) {
  push_byte(buffer, tag);
  push_byte(buffer, 0); // Size placeholder
  push_byte(buffer, 0);
  push_byte(buffer, 0);
  return index + 4;
}

json_t *VultJsonSerializerJansson::binaryToJson(CustomBuffer &buffer) {
  // Reset buffer state
  buffer.error = false;
  buffer.calculate_size = false;

  // Parse type descriptions first
  parseTypeDescriptions(buffer);

  // Find start of data section
  int32_t data_index = goto_data(buffer);
  if (data_index < 0) {
    throw std::runtime_error("No data section found in binary buffer");
  }

  // Parse the main data object
  json_t *result = parseDataValue(buffer, data_index);

  if (buffer.error || !result) {
    if (result)
      json_decref(result);
    throw std::runtime_error("Error parsing binary data");
  }

  return result;
}

CustomBuffer VultJsonSerializerJansson::jsonToBinary(json_t *data, const std::string &main_type_name) {
  if (!data) {
    throw std::runtime_error("Invalid JSON data (null pointer)");
  }

  CustomBuffer buffer;
  buffer.data.clear();
  buffer.error = false;
  buffer.calculate_size = true;

  // Register main type based on JSON structure
  if (json_is_object(data)) {
    TypeInfo main_type;
    main_type.name = main_type_name;

    // Get object keys in order
    const char *key;
    json_t *value;
    json_object_foreach(data, key, value) {
      main_type.fields.push_back(key);
    }

    type_registry[main_type_name] = main_type;
  }

  // First pass: calculate size
  int32_t index = 0;

  // Write type descriptions (size calculation)
  index = writeTypeDescriptions(buffer, index);

  // Write data (size calculation)
  index = writeDataValue(buffer, index, data);

  // Second pass: write actual data
  buffer.data.resize(static_cast<size_t>(index));
  buffer.calculate_size = false;

  index = 0;
  index = writeTypeDescriptions(buffer, index);
  index = writeDataValue(buffer, index, data);

  return buffer;
}

char *VultJsonSerializerJansson::toJsonString(CustomBuffer &buffer, size_t flags) {
  JsonPtr result(binaryToJson(buffer));

  // Create metadata object
  JsonPtr output(json_object());

  // Add metadata
  JsonPtr metadata(json_object());
  json_object_set_new(metadata.get(), "format", json_string("vult_serialization"));
  json_object_set_new(metadata.get(), "version", json_string("1.0"));

  // Add type information
  JsonPtr types(json_object());
  for (const auto &[type_name, type_info] : type_registry) {
    JsonPtr fields(json_array());
    for (const auto &field : type_info.fields) {
      json_array_append_new(fields.get(), json_string(field.c_str()));
    }
    json_object_set_new(types.get(), type_name.c_str(), fields.release());
  }
  json_object_set_new(metadata.get(), "types", types.release());

  json_object_set_new(output.get(), "metadata", metadata.release());
  json_object_set_new(output.get(), "data", result.release());

  return json_dumps(output.get(), flags);
}

CustomBuffer VultJsonSerializerJansson::fromJsonString(const char *json_str) {
  json_error_t error;
  JsonPtr parsed(json_loads(json_str, 0, &error));

  if (!parsed) {
    std::stringstream ss;
    ss << "JSON parse error: " << error.text << " at line " << error.line
       << ", column " << error.column;
    throw std::runtime_error(ss.str());
  }

  // Check if it has metadata format
  json_t *data_section = parsed.get();
  json_t *metadata = json_object_get(parsed.get(), "metadata");

  if (metadata) {
    // Load type information from metadata
    json_t *types = json_object_get(metadata, "types");
    if (types && json_is_object(types)) {
      type_registry.clear();

      const char *type_name;
      json_t *fields;
      json_object_foreach(types, type_name, fields) {
        if (json_is_array(fields)) {
          TypeInfo info;
          info.name = type_name;

          size_t field_count = json_array_size(fields);
          for (size_t i = 0; i < field_count; i++) {
            json_t *field = json_array_get(fields, i);
            if (json_is_string(field)) {
              info.fields.push_back(json_string_value(field));
            }
          }

          type_registry[type_name] = info;
        }
      }
    }

    // Get actual data section
    data_section = json_object_get(parsed.get(), "data");
    if (!data_section) {
      throw std::runtime_error("JSON with metadata is missing 'data' section");
    }
  }

  return jsonToBinary(data_section);
}

CustomBuffer VultJsonSerializerJansson::fromJsonFile(const char *filename) {
  json_error_t error;
  JsonPtr parsed(json_load_file(filename, 0, &error));

  if (!parsed) {
    std::stringstream ss;
    ss << "Failed to load JSON file '" << filename << "': " << error.text
       << " at line " << error.line << ", column " << error.column;
    throw std::runtime_error(ss.str());
  }

  // Reuse fromJsonString logic by converting to string first
  char *json_str = json_dumps(parsed.get(), JSON_COMPACT);
  if (!json_str) {
    throw std::runtime_error("Failed to convert JSON to string");
  }

  CustomBuffer result;
  try {
    result = fromJsonString(json_str);
  } catch (...) {
    free(json_str);
    throw;
  }

  free(json_str);
  return result;
}

void VultJsonSerializerJansson::registerType(const std::string &type_name,
                                             const std::vector<std::string> &field_names) {
  TypeInfo info;
  info.name = type_name;
  info.fields = field_names;
  type_registry[type_name] = info;
}

void VultJsonSerializerJansson::clearTypeRegistry() {
  type_registry.clear();
}

std::vector<std::string> VultJsonSerializerJansson::getTypeFields(const std::string &type_name) const {
  auto it = type_registry.find(type_name);
  if (it != type_registry.end()) {
    return it->second.fields;
  }
  return {};
}

// Helper function to convert C++ values to json_t*
namespace {
json_t *value_to_json(int val) { return json_integer(val); }
json_t *value_to_json(double val) { return json_real(val); }
json_t *value_to_json(float val) { return json_real(val); }
json_t *value_to_json(bool val) { return val ? json_true() : json_false(); }
json_t *value_to_json(const std::string &val) { return json_string(val.c_str()); }
json_t *value_to_json(const char *val) { return json_string(val); }
} // namespace

template <typename T>
json_t *VultJsonSerializerJansson::createJsonObject(const std::map<std::string, T> &values) {
  json_t *obj = json_object();
  if (!obj)
    return nullptr;

  for (const auto &[key, value] : values) {
    json_t *json_val = value_to_json(value);
    if (json_val) {
      json_object_set_new(obj, key.c_str(), json_val);
    }
  }

  return obj;
}

template <typename T>
CustomBuffer VultJsonSerializerJansson::mapToBinary(const std::map<std::string, T> &values,
                                                    const std::string &main_type_name) {
  JsonPtr obj(createJsonObject(values));
  if (!obj) {
    throw std::runtime_error("Failed to create JSON object from map");
  }

  return jsonToBinary(obj.get(), main_type_name);
}

std::map<std::string, std::string> VultJsonSerializerJansson::jsonStringToMap(const char *json_str) {
  std::map<std::string, std::string> result;

  json_error_t error;
  JsonPtr parsed(json_loads(json_str, 0, &error));

  if (!parsed) {
    std::stringstream ss;
    ss << "JSON parse error: " << error.text << " at line " << error.line
       << ", column " << error.column;
    throw std::runtime_error(ss.str());
  }

  if (!json_is_object(parsed.get())) {
    throw std::runtime_error("JSON root is not an object");
  }

  const char *key;
  json_t *value;
  json_object_foreach(parsed.get(), key, value) {
    char *str_value = json_dumps(value, JSON_COMPACT);
    if (str_value) {
      result[key] = str_value;
      free(str_value);
    }
  }

  return result;
}

// Explicit template instantiations for common types
template json_t *VultJsonSerializerJansson::createJsonObject<int>(const std::map<std::string, int> &);
template json_t *VultJsonSerializerJansson::createJsonObject<double>(const std::map<std::string, double> &);
template json_t *VultJsonSerializerJansson::createJsonObject<float>(const std::map<std::string, float> &);
template json_t *VultJsonSerializerJansson::createJsonObject<bool>(const std::map<std::string, bool> &);
template json_t *VultJsonSerializerJansson::createJsonObject<std::string>(const std::map<std::string, std::string> &);

template CustomBuffer VultJsonSerializerJansson::mapToBinary<int>(const std::map<std::string, int> &, const std::string &);
template CustomBuffer VultJsonSerializerJansson::mapToBinary<double>(const std::map<std::string, double> &, const std::string &);
template CustomBuffer VultJsonSerializerJansson::mapToBinary<float>(const std::map<std::string, float> &, const std::string &);
template CustomBuffer VultJsonSerializerJansson::mapToBinary<bool>(const std::map<std::string, bool> &, const std::string &);
template CustomBuffer VultJsonSerializerJansson::mapToBinary<std::string>(const std::map<std::string, std::string> &, const std::string &);