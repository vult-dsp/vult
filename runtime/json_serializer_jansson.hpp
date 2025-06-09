#ifndef VULT_JSON_SERIALIZER_JANSSON_HPP
#define VULT_JSON_SERIALIZER_JANSSON_HPP

/*
  Vult JSON Serializer (Jansson) - Convert between Vult binary format and JSON

  The MIT License (MIT)
  Copyright (c) 2014-2024 Leonardo Laguna Ruiz

  This module provides bidirectional conversion between Vult's native binary
  serialization format and JSON using the Jansson C library. It enables
  human-readable configuration files, preset management, debugging, and API integration.

  =============================================================================
  QUICK START USAGE
  =============================================================================

  1. Include headers:

  #include "json_serializer_jansson.hpp"
  #include <jansson.h>  // Required dependency
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
  VultJsonSerializerJansson serializer;
  json_t* result = serializer.binaryToJson(binary_buffer);

  // Pretty print or save to file
  char* json_str = json_dumps(result, JSON_INDENT(2));
  printf("%s\n", json_str);
  free(json_str);
  json_decref(result);


  3. Convert JSON to Vult binary format:

  Method A: Using convenience functions (easier):

  std::map<std::string, double> config = {
      {"enabled", 1.0},
      {"gain", 0.8},
      {"cutoff", 1000.0},
      {"resonance", 0.7}
  };

  VultJsonSerializerJansson serializer;
  CustomBuffer binary = serializer.mapToBinary(config, "MyVultType_main_type");

  Method B: Using JSON strings (also easy):

  const char* json_str = R"({
      "enabled": true,
      "gain": 0.8,
      "filter": {
          "cutoff": 1000.0,
          "resonance": 0.7
      }
  })";

  VultJsonSerializerJansson serializer;
  CustomBuffer binary = serializer.fromJsonString(json_str);

  Method C: Manual JSON creation (more control):

  json_t* config = json_object();
  json_object_set_new(config, "enabled", json_true());
  json_object_set_new(config, "gain", json_real(0.8));

  json_t* filter = json_object();
  json_object_set_new(filter, "cutoff", json_real(1000.0));
  json_object_set_new(filter, "resonance", json_real(0.7));
  json_object_set_new(config, "filter", filter);

  VultJsonSerializerJansson serializer;
  CustomBuffer binary = serializer.jsonToBinary(config, "MyVultType_main_type");
  json_decref(config);

  // Use with Vult deserializer
  MyVultType_main_type result;
  deserialize_data(binary, MyVultType_main_type_deserialize_data,
                   "MyVultType_main_type", result);


  4. Load JSON from file:

  json_error_t error;
  json_t* json_data = json_load_file("preset.json", 0, &error);
  if (json_data) {
      VultJsonSerializerJansson serializer;
      CustomBuffer binary = serializer.jsonToBinary(json_data);
      json_decref(json_data);
  }


  =============================================================================
  CONVENIENCE USAGE EXAMPLES
  =============================================================================

  1. Simple configuration with C++ map:

  std::map<std::string, double> audio_config = {
      {"sample_rate", 44100.0},
      {"buffer_size", 512.0},
      {"master_gain", 0.8},
      {"reverb_amount", 0.3}
  };

  VultJsonSerializerJansson serializer;
  CustomBuffer binary = serializer.mapToBinary(audio_config, "AudioEngine");

  // Use with your Vult engine
  AudioEngine_main_type engine;
  deserialize_data(binary, AudioEngine_main_type_deserialize_data,
                   "AudioEngine_main_type", engine);

  2. Load configuration from JSON file:

  VultJsonSerializerJansson serializer;
  CustomBuffer binary = serializer.fromJsonFile("presets/my_preset.json");

  3. Convert JSON string directly:

  const char* config = R"({
      "oscillator": {"frequency": 440.0, "amplitude": 0.8},
      "filter": {"cutoff": 2000.0, "resonance": 0.3},
      "envelope": {"attack": 0.1, "decay": 0.2, "sustain": 0.7, "release": 0.5}
  })";

  CustomBuffer binary = serializer.fromJsonString(config);

  4. Extract values from JSON into C++ map:

  auto values = serializer.jsonStringToMap(R"({"gain": 0.8, "enabled": true})");
  std::cout << "Gain: " << values["gain"] << std::endl;        // "0.8"
  std::cout << "Enabled: " << values["enabled"] << std::endl;  // "true"

  =============================================================================
  JANSSON SPECIFIC NOTES
  =============================================================================

  Jansson uses reference counting for memory management:
  - json_incref() increases reference count
  - json_decref() decreases reference count (frees when reaches 0)
  - Functions ending with _new return new references
  - Most getter functions return borrowed references

  Key Jansson functions:
  - json_object() - Create object
  - json_array() - Create array
  - json_string() - Create string
  - json_integer() - Create integer
  - json_real() - Create real number
  - json_true()/json_false() - Create boolean
  - json_object_set_new() - Set object field (steals reference)
  - json_array_append_new() - Append to array (steals reference)

  =============================================================================
  ERROR HANDLING
  =============================================================================

  The serializer uses exceptions for C++ style error handling:

  try {
      VultJsonSerializerJansson serializer;
      json_t* result = serializer.binaryToJson(buffer);
      // Use result...
      json_decref(result);
  } catch (const std::runtime_error& e) {
      std::cerr << "Conversion failed: " << e.what() << std::endl;
  }


  For Jansson-specific errors:

  json_error_t error;
  json_t* data = json_loads(json_string, 0, &error);
  if (!data) {
      std::cerr << "JSON parse error: " << error.text << std::endl;
      std::cerr << "At line " << error.line << ", column " << error.column << std::endl;
  }


  =============================================================================
  BUILDING AND DEPENDENCIES
  =============================================================================

  Required dependencies:
  - jansson library (C library)
  - C++11 or later

  Installing Jansson:

  # Ubuntu/Debian
  apt-get install libjansson-dev

  # macOS
  brew install jansson

  # From source
  wget https://github.com/akheron/jansson/releases/download/v2.14/jansson-2.14.tar.gz
  tar xzf jansson-2.14.tar.gz
  cd jansson-2.14
  ./configure && make && make install


  CMake example:

  find_package(PkgConfig REQUIRED)
  pkg_check_modules(JANSSON REQUIRED jansson)

  add_executable(my_app
      main.cpp
      json_serializer_jansson.cpp
      vultin.cpp
      my_vult_engine.cpp
  )

  target_include_directories(my_app PRIVATE ${JANSSON_INCLUDE_DIRS})
  target_link_libraries(my_app ${JANSSON_LIBRARIES})
  target_compile_features(my_app PRIVATE cxx_std_11)

  =============================================================================
*/

#include "vultin.hpp"
#include <jansson.h>
#include <string>
#include <vector>
#include <map>
#include <stdexcept>
#include <memory>

/**
   RAII wrapper for json_t* to ensure proper cleanup
 */
class JsonPtr {
private:
  json_t *ptr;
  bool owned;

public:
  explicit JsonPtr(json_t *p = nullptr, bool take_ownership = true)
      : ptr(p), owned(take_ownership && p != nullptr) {}

  ~JsonPtr() {
    if (owned && ptr) {
      json_decref(ptr);
    }
  }

  // Disable copy
  JsonPtr(const JsonPtr &) = delete;
  JsonPtr &operator=(const JsonPtr &) = delete;

  // Enable move
  JsonPtr(JsonPtr &&other) noexcept : ptr(other.ptr), owned(other.owned) {
    other.ptr = nullptr;
    other.owned = false;
  }

  JsonPtr &operator=(JsonPtr &&other) noexcept {
    if (this != &other) {
      if (owned && ptr) {
        json_decref(ptr);
      }
      ptr = other.ptr;
      owned = other.owned;
      other.ptr = nullptr;
      other.owned = false;
    }
    return *this;
  }

  json_t *get() const { return ptr; }
  json_t *release() {
    owned = false;
    return ptr;
  }
  void reset(json_t *p = nullptr, bool take_ownership = true) {
    if (owned && ptr) {
      json_decref(ptr);
    }
    ptr = p;
    owned = take_ownership && p != nullptr;
  }

  operator bool() const { return ptr != nullptr; }
  json_t *operator->() const { return ptr; }
};

/**
   Vult JSON Serializer using Jansson

   Provides bidirectional conversion between Vult's binary serialization format
   and JSON using the Jansson C library. Supports all Vult data types including
   nested structs and arrays.

   Key features:
   - Type-safe conversion using tagged binary format
   - Preserves field names through type descriptions
   - Memory-safe implementation with RAII wrappers
   - Round-trip conversion with full data integrity
   - Human-readable JSON with optional metadata
   - Lighter weight than nlohmann/json
 */
class VultJsonSerializerJansson {
private:
  struct TypeInfo {
    std::string name;
    std::vector<std::string> fields;
  };

  std::map<std::string, TypeInfo> type_registry;

  // Parse type descriptions from binary buffer
  void parseTypeDescriptions(CustomBuffer &buffer);

  // Convert binary data section to JSON recursively
  json_t *parseDataValue(CustomBuffer &buffer, int32_t &index, const std::string &expected_type = "");

  // Convert JSON to binary data recursively
  int32_t writeDataValue(CustomBuffer &buffer, int32_t index, json_t *value);

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
     @return JSON object representing the data (caller must json_decref)
     @throws std::runtime_error if binary format is invalid

     Example:
     VultJsonSerializerJansson serializer;
     json_t* result = serializer.binaryToJson(vult_buffer);
     char* str = json_dumps(result, JSON_INDENT(2));
     printf("%s\n", str);
     free(str);
     json_decref(result);
   */
  json_t *binaryToJson(CustomBuffer &buffer);

  /**
     Convert JSON to Vult binary format

     @param data JSON object to convert (borrowed reference)
     @param main_type_name Name of the main type (for type description)
     @return CustomBuffer containing binary data
     @throws std::runtime_error if JSON structure is invalid

     Example:
     json_t* config = json_pack("{s:f, s:b}", "gain", 0.8, "enabled", 1);
     VultJsonSerializerJansson serializer;
     CustomBuffer binary = serializer.jsonToBinary(config, "MyType");
     json_decref(config);
   */
  CustomBuffer jsonToBinary(json_t *data, const std::string &main_type_name = "MainType");

  /**
     Convert binary to JSON string with metadata

     @param buffer Binary buffer to convert
     @param flags Jansson encoding flags (e.g., JSON_INDENT(2))
     @return JSON string with metadata (caller must free())

     Example:
     VultJsonSerializerJansson serializer;
     char* json_str = serializer.toJsonString(buffer, JSON_INDENT(2) | JSON_SORT_KEYS);
     FILE* f = fopen("config.json", "w");
     fprintf(f, "%s", json_str);
     fclose(f);
     free(json_str);
   */
  char *toJsonString(CustomBuffer &buffer, size_t flags = JSON_INDENT(2));

  /**
     Load JSON string and convert to binary

     @param json_str JSON string (with or without metadata)
     @return CustomBuffer containing binary data
     @throws std::runtime_error if JSON syntax is invalid or conversion fails

     Example:
     const char* json_str = "{\"gain\": 0.8, \"enabled\": true}";
     VultJsonSerializerJansson serializer;
     CustomBuffer binary = serializer.fromJsonString(json_str);
   */
  CustomBuffer fromJsonString(const char *json_str);

  /**
     Load JSON file and convert to binary

     @param filename Path to JSON file
     @return CustomBuffer containing binary data
     @throws std::runtime_error if file cannot be read or JSON is invalid

     Example:
     VultJsonSerializerJansson serializer;
     CustomBuffer binary = serializer.fromJsonFile("config.json");
   */
  CustomBuffer fromJsonFile(const char *filename);

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

#endif // VULT_JSON_SERIALIZER_JANSSON_HPP