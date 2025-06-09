/*
 * Vult JSON Serializer Implementation
 *
 * The MIT License (MIT)
 * Copyright (c) 2014-2024 Leonardo Laguna Ruiz
 *
 * This file implements the VultJsonSerializer class for converting between
 * Vult's binary serialization format and JSON.
 */

#include "json_serializer.hpp"
#include <sstream>
#include <iomanip>

void VultJsonSerializer::parseTypeDescriptions(CustomBuffer &buffer) {
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

json VultJsonSerializer::parseDataValue(CustomBuffer &buffer, int32_t &index, const std::string &expected_type) {
  uint8_t tag = read_byte(buffer, index);

  switch (tag) {
  case SMALL_INT_TAG: {
    int32_t value = deserialize_int(buffer, index);
    index = next_object(buffer, index);
    return json(value);
  }
  case INT_TAG: {
    int32_t value = deserialize_int(buffer, index);
    index = next_object(buffer, index);
    return json(value);
  }
  case FLOAT_TAG: {
    float value = deserialize_float(buffer, index);
    index = next_object(buffer, index);
    return json(value);
  }
  case STRING_TAG: {
    std::string value = deserialize_string(buffer, index);
    index = next_object(buffer, index);
    return json(value);
  }
  case BLOCK_TAG: {
    json obj = json::object();
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

        json field_value = parseDataValue(buffer, index);
        obj[field_name] = field_value;
      }
    } else {
      // Unknown type, parse as generic object with numbered fields
      int field_count = 0;
      while (index < block_start + block_size_val) {
        json field_value = parseDataValue(buffer, index);
        obj["field_" + std::to_string(field_count++)] = field_value;
      }
    }

    return obj;
  }
  case ARRAY_TAG: {
    json arr = json::array();
    int32_t array_start = index;
    int32_t array_size_val = block_size(buffer, index);
    index += 4; // Skip header

    // Read number of elements
    int32_t num_elements = deserialize_int(buffer, index);
    index = next_object(buffer, index);

    // Parse each element
    for (int32_t i = 0; i < num_elements && index < array_start + array_size_val; i++) {
      json element = parseDataValue(buffer, index);
      arr.push_back(element);
    }

    return arr;
  }
  default:
    // Unknown tag, skip
    index = next_object(buffer, index);
    return json(nullptr);
  }
}

int32_t VultJsonSerializer::writeDataValue(CustomBuffer &buffer, int32_t index, const json &value) {
  if (value.is_null()) {
    // Handle null - write as zero int
    return push_int(buffer, index, 0);
  } else if (value.is_boolean()) {
    // Convert boolean to int (0 or 1)
    return push_int(buffer, index, value.get<bool>() ? 1 : 0);
  } else if (value.is_number_integer()) {
    // Integer value
    return push_int(buffer, index, value.get<int32_t>());
  } else if (value.is_number_float()) {
    // Float value
    return push_float(buffer, index, value.get<float>());
  } else if (value.is_string()) {
    // String value
    return push_string(buffer, index, value.get<std::string>());
  } else if (value.is_array()) {
    // Array value
    int32_t start = index;
    index = push_array(buffer, index, static_cast<int32_t>(value.size()));

    for (const auto &element : value) {
      index = writeDataValue(buffer, index, element);
    }

    update_size(buffer, start, index - start);
    return index;
  } else if (value.is_object()) {
    // Object/struct value
    int32_t start = index;
    index = push_block_header(buffer, index);

    for (const auto &[key, val] : value.items()) {
      index = writeDataValue(buffer, index, val);
    }

    update_size(buffer, start, index - start);
    return index;
  }

  return index;
}

int32_t VultJsonSerializer::writeTypeDescriptions(CustomBuffer &buffer, int32_t index) {
  for (const auto &[type_name, type_info] : type_registry) {
    index = serialize_type_descr(buffer, index, type_name, type_info.fields);
  }
  return index;
}

int32_t VultJsonSerializer::serialize_type_descr(CustomBuffer &buffer, int32_t index,
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

int32_t VultJsonSerializer::push_header(CustomBuffer &buffer, int32_t index, uint8_t tag) {
  push_byte(buffer, tag);
  push_byte(buffer, 0); // Size placeholder
  push_byte(buffer, 0);
  push_byte(buffer, 0);
  return index + 4;
}

json VultJsonSerializer::binaryToJson(CustomBuffer &buffer) {
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
  json result = parseDataValue(buffer, data_index);

  if (buffer.error) {
    throw std::runtime_error("Error parsing binary data");
  }

  return result;
}

CustomBuffer VultJsonSerializer::jsonToBinary(const json &data, const std::string &main_type_name) {
  CustomBuffer buffer;
  buffer.data.clear();
  buffer.error = false;
  buffer.calculate_size = true;

  // Register main type based on JSON structure
  if (data.is_object()) {
    TypeInfo main_type;
    main_type.name = main_type_name;
    for (const auto &[key, value] : data.items()) {
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

std::string VultJsonSerializer::toJsonString(CustomBuffer &buffer, int indent) {
  json result = binaryToJson(buffer);

  // Add metadata
  json output;
  output["metadata"] = json::object();
  output["metadata"]["format"] = "vult_serialization";
  output["metadata"]["version"] = "1.0";
  output["metadata"]["types"] = json::object();

  for (const auto &[type_name, type_info] : type_registry) {
    output["metadata"]["types"][type_name] = type_info.fields;
  }

  output["data"] = result;

  return output.dump(indent);
}

CustomBuffer VultJsonSerializer::fromJsonString(const std::string &json_str) {
  json parsed = json::parse(json_str);

  // Check if it has metadata format
  if (parsed.contains("data") && parsed.contains("metadata")) {
    // Load type information from metadata
    if (parsed["metadata"].contains("types")) {
      type_registry.clear();
      for (const auto &[type_name, fields] : parsed["metadata"]["types"].items()) {
        TypeInfo info;
        info.name = type_name;
        for (const auto &field : fields) {
          info.fields.push_back(field.get<std::string>());
        }
        type_registry[type_name] = info;
      }
    }
    return jsonToBinary(parsed["data"]);
  } else {
    // Direct JSON data
    return jsonToBinary(parsed);
  }
}

void VultJsonSerializer::registerType(const std::string &type_name, const std::vector<std::string> &field_names) {
  TypeInfo info;
  info.name = type_name;
  info.fields = field_names;
  type_registry[type_name] = info;
}

void VultJsonSerializer::clearTypeRegistry() {
  type_registry.clear();
}

std::vector<std::string> VultJsonSerializer::getTypeFields(const std::string &type_name) const {
  auto it = type_registry.find(type_name);
  if (it != type_registry.end()) {
    return it->second.fields;
  }
  return {};
}