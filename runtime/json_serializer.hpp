#ifndef VULT_JSON_SERIALIZER_HPP
#define VULT_JSON_SERIALIZER_HPP

/*
  Vult JSON Serializer - Convert between Vult binary format and JSON

  The MIT License (MIT)
  Copyright (c) 2014-2024 Leonardo Laguna Ruiz

  This module provides bidirectional conversion between Vult's native binary
  serialization format and JSON. It enables human-readable configuration files,
  preset management, debugging, and API integration.

  =============================================================================
  QUICK START USAGE
  =============================================================================

  1. Include headers:

  #include "json_serializer.hpp"
  #include <nlohmann/json.hpp>  // Required dependency
  #include "your_vult_engine.h" // Your generated Vult code


  2. Convert Vult binary data to JSON:

  // Start with Vult structure
  MyVultType_main_type vult_data;
  MyVultType_main_type_init(vult_data);

  // Serialize to Vult binary format
  CustomBuffer binary_buffer;
  serialize_data(binary_buffer,
                 MyVultType_main_type_serialize_type_descr,
                 MyVultType_main_type_serialize_data,
                 vult_data);

  // Convert to JSON
  VultJsonSerializer serializer;
  json result = serializer.binaryToJson(binary_buffer);

  // Pretty print or save to file
  std::cout << result.dump(2) << std::endl;
  std::ofstream("config.json") << serializer.toJsonString(binary_buffer, 2);


  3. Convert JSON to Vult binary format:

  // Create or load JSON data
  json config = {
      {"enabled", true},
      {"gain", 0.8},
      {"filter", {
          {"cutoff", 1000.0},
          {"resonance", 0.7}
      }}
  };

  // Convert to Vult binary
  VultJsonSerializer serializer;
  CustomBuffer binary = serializer.jsonToBinary(config, "MyVultType_main_type");

  // Use with Vult deserializer
  MyVultType_main_type result;
  deserialize_data(binary, MyVultType_main_type_deserialize_data,
                   "MyVultType_main_type", result);


  4. Load JSON from file:

  std::ifstream file("preset.json");
  std::string json_content((std::istreambuf_iterator<char>(file)),
                          std::istreambuf_iterator<char>());

  VultJsonSerializer serializer;
  CustomBuffer binary = serializer.fromJsonString(json_content);


  =============================================================================
  VULT BINARY FORMAT STRUCTURE
  =============================================================================

  The Vult serialization format consists of two main sections:

  [Type Descriptions Section]
  Contains metadata about struct layouts and field names. This enables
  the JSON converter to use meaningful field names instead of generic indices.

  [Data Section]
  Contains the actual serialized values using a tagged format where each
  value has a type identifier.

  Binary Format Tags:
  - 't' (TYPE_TAG): Type description with field names
  - 'b' (BLOCK_TAG): Block/struct container
  - 's' (STRING_TAG): String value (null-terminated)
  - 'f' (FLOAT_TAG): 32-bit IEEE 754 float
  - 'i' (SMALL_INT_TAG): 8-bit integer (-128 to 127)
  - 'I' (INT_TAG): 32-bit signed integer
  - 'a' (ARRAY_TAG): Array container with element count

  Each tagged item (except fixed-size int/float) has the format:
  [tag][3-byte size][data...]

  =============================================================================
  JSON FORMAT STRUCTURE
  =============================================================================

  Simple format (direct data only):

  {
    "enabled": 1,
    "master_gain": 0.5,
    "filter": {
      "cutoff": 1000.0,
      "resonance": 0.7
    },
    "oscillators": [440.0, 880.0, 1320.0]
  }


  Extended format (with metadata for accurate round-trips):

  {
    "metadata": {
      "format": "vult_serialization",
      "version": "1.0",
      "types": {
        "MainType": ["enabled", "master_gain", "filter"],
        "FilterType": ["cutoff", "resonance"]
      }
    },
    "data": {
      "enabled": 1,
      "master_gain": 0.5,
      "filter": {"cutoff": 1000.0, "resonance": 0.7}
    }
  }


  =============================================================================
  DATA TYPE MAPPING
  =============================================================================

  | Vult Type | JSON Type | Notes                         |
  |-----------|----------|--------------------------------|
  | int       | number   | 32-bit signed integer          |
  | real      | number   | 32-bit IEEE 754 float          |
  | bool      | number   | 0 = false, 1 = true            |
  | string    | string   | UTF-8 string                   |
  | struct    | object   | JSON object with field names   |
  | array     | array    | JSON array                     |

  =============================================================================
  PRACTICAL APPLICATIONS
  =============================================================================

  1. Configuration Files:

  // Create human-editable config
  json synth_config = {
      {"sample_rate", 44100},
      {"buffer_size", 512},
      {"oscillator", {
          {"waveform", "sawtooth"},
          {"frequency", 440.0},
          {"amplitude", 0.8}
      }},
      {"filter", {
          {"type", "lowpass"},
          {"cutoff", 2000.0},
          {"resonance", 0.3}
      }}
  };
  std::ofstream("synth_config.json") << synth_config.dump(2);


  2. Preset Management:

  // Save current state as preset
  CustomBuffer current_state;
  serialize_data(current_state, ..., synth_engine);

  VultJsonSerializer serializer;
  json preset = serializer.binaryToJson(current_state);
  preset["metadata"]["name"] = "My Favorite Sound";
  preset["metadata"]["author"] = "User Name";
  preset["metadata"]["date"] = "2024-06-09";

  std::ofstream("presets/favorite.json") << preset.dump(2);

  // Later: load preset
  std::ifstream file("presets/favorite.json");
  json loaded_preset = json::parse(file);
  CustomBuffer preset_binary = serializer.fromJsonString(loaded_preset.dump());
  deserialize_data(preset_binary, ..., synth_engine);


  3. API Integration:

  // Web API endpoint
  void handle_save_state(const json& request) {
      VultJsonSerializer serializer;
      CustomBuffer binary = serializer.jsonToBinary(request["state"]);

      // Apply to Vult engine
      deserialize_data(binary, ..., audio_engine);

      // Send confirmation
      json response = {{"status", "success"}};
      send_response(response.dump());
  }


  4. Debugging and Inspection:

  // Inspect binary data in human-readable form
  void debug_vult_state(const CustomBuffer& mystery_buffer) {
      VultJsonSerializer serializer;
      json readable = serializer.binaryToJson(mystery_buffer);

      std::cout << "Current engine state:" << std::endl;
      std::cout << readable.dump(2) << std::endl;

      // Check specific values
      if (readable.contains("filter") && readable["filter"].contains("cutoff")) {
          double cutoff = readable["filter"]["cutoff"];
          std::cout << "Filter cutoff: " << cutoff << " Hz" << std::endl;
      }
  }


  =============================================================================
  ERROR HANDLING
  =============================================================================

  The serializer uses exceptions for error reporting:

  try {
      VultJsonSerializer serializer;
      json result = serializer.binaryToJson(buffer);
      // Success
  } catch (const std::runtime_error& e) {
      std::cerr << "Conversion failed: " << e.what() << std::endl;
  } catch (const json::parse_error& e) {
      std::cerr << "JSON parsing failed: " << e.what() << std::endl;
  }


  Common error conditions:
  - Malformed binary buffer (missing type descriptions or data section)
  - Invalid JSON syntax
  - Type mismatches between JSON and expected Vult types
  - Buffer overflow or underflow during parsing

  =============================================================================
  BUILDING AND DEPENDENCIES
  =============================================================================

  Required dependencies:
  - nlohmann/json (header-only library)
  - C++17 or later

  CMake example:

  find_package(nlohmann_json REQUIRED)

  add_executable(my_app
      main.cpp
      json_serializer.cpp
      vultin.cpp
      my_vult_engine.cpp
  )

  target_link_libraries(my_app nlohmann_json::nlohmann_json)
  target_compile_features(my_app PRIVATE cxx_std_17)


  Manual installation of nlohmann/json:

  # Download single header file
  wget https://github.com/nlohmann/json/releases/download/v3.11.2/json.hpp
  # Or clone repository
  git clone https://github.com/nlohmann/json.git


  =============================================================================
  PERFORMANCE CONSIDERATIONS
  =============================================================================

  - Binary format parsing is optimized for sequential access
  - JSON conversion adds readable overhead but maintains type safety
  - Memory usage scales linearly with data size
  - Round-trip conversion preserves all data types and precision
  - Large arrays and deeply nested structures are supported efficiently

  For high-performance scenarios:
  - Use binary format directly for real-time audio processing
  - Use JSON format for configuration, presets, and debugging
  - Cache converted data when possible to avoid repeated parsing

  =============================================================================
*/

#include "vultin.hpp"
#include <nlohmann/json.hpp>
#include <string>
#include <vector>
#include <map>
#include <stdexcept>

using json = nlohmann::json;

/**
   Vult JSON Serializer

   Provides bidirectional conversion between Vult's binary serialization format
   and JSON. Supports all Vult data types including nested structs and arrays.

   Key features:
   - Type-safe conversion using tagged binary format
   - Preserves field names through type descriptions
   - Memory-safe implementation (no type punning)
   - Round-trip conversion with full data integrity
   - Human-readable JSON with optional metadata
 */
class VultJsonSerializer {
private:
  struct TypeInfo {
    std::string name;
    std::vector<std::string> fields;
  };

  std::map<std::string, TypeInfo> type_registry;

  // Parse type descriptions from binary buffer
  void parseTypeDescriptions(CustomBuffer &buffer);

  // Convert binary data section to JSON recursively
  json parseDataValue(CustomBuffer &buffer, int32_t &index, const std::string &expected_type = "");

  // Convert JSON to binary data recursively
  int32_t writeDataValue(CustomBuffer &buffer, int32_t index, const json &value);

  // Write type descriptions to buffer
  int32_t writeTypeDescriptions(CustomBuffer &buffer, int32_t index);

  // Helper function to serialize type description
  int32_t serialize_type_descr(CustomBuffer &buffer, int32_t index, const std::string &type_name,
                               const std::vector<std::string> &field_names);

  int32_t push_header(CustomBuffer &buffer, int32_t index, uint8_t tag);

public:
  /**
     Convert Vult binary data to JSON

     @param buffer Binary buffer containing Vult serialized data
     @return JSON object representing the data
     @throws std::runtime_error if binary format is invalid

     Example:
     VultJsonSerializer serializer;
     json result = serializer.binaryToJson(vult_buffer);
     std::cout << result.dump(2) << std::endl;
   */
  json binaryToJson(CustomBuffer &buffer);

  /**
     Convert JSON to Vult binary format

     @param data JSON object to convert
     @param main_type_name Name of the main type (for type description)
     @return CustomBuffer containing binary data
     @throws std::runtime_error if JSON structure is invalid

     Example:
     json config = {{"gain", 0.8}, {"enabled", true}};
     VultJsonSerializer serializer;
     CustomBuffer binary = serializer.jsonToBinary(config, "MyType");
   */
  CustomBuffer jsonToBinary(const json &data, const std::string &main_type_name = "MainType");

  /**
     Convert binary to JSON string with metadata

     @param buffer Binary buffer to convert
     @param indent Number of spaces for JSON indentation
     @return JSON string with metadata section

     Example:
     std::string json_str = serializer.toJsonString(buffer, 2);
     std::ofstream("config.json") << json_str;
   */
  std::string toJsonString(CustomBuffer &buffer, int indent = 2);

  /**
     Load JSON string and convert to binary

     @param json_str JSON string (with or without metadata)
     @return CustomBuffer containing binary data
     @throws json::parse_error if JSON syntax is invalid
     @throws std::runtime_error if conversion fails

     Example:
     std::ifstream file("config.json");
     std::string content((std::istreambuf_iterator<char>(file)),
                        std::istreambuf_iterator<char>());
     CustomBuffer binary = serializer.fromJsonString(content);
   */
  CustomBuffer fromJsonString(const std::string &json_str);

  /**
     Register a custom type manually

     @param type_name Name of the type
     @param field_names Vector of field names in order

     Example:
     serializer.registerType("FilterType", {"cutoff", "resonance", "enabled"});
   */
  void registerType(const std::string &type_name, const std::vector<std::string> &field_names);

  /**
     Clear all registered type information
   */
  void clearTypeRegistry();

  /**
     Get information about a registered type

     @param type_name Name of the type to query
     @return Vector of field names, empty if type not found
   */
  std::vector<std::string> getTypeFields(const std::string &type_name) const;
};

#endif // VULT_JSON_SERIALIZER_HPP