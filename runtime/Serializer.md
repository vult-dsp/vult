# Vult Binary Serialization Format

This document describes the binary serialization format used by the Vult language for persisting state data marked with `@[save]` tags.

## Overview

The Vult serialization format is a compact, type-safe binary format designed for:
- Fast serialization/deserialization of audio DSP state
- Type safety with tagged values
- Efficient memory usage
- Platform independence
- Human-readable conversion via JSON

## Format Structure

The serialization format consists of two main sections:

```
[Type Descriptions Section]
[Data Section]
```

### 1. Type Descriptions Section

Contains metadata about struct types and their field names. This enables:
- Field name preservation for JSON conversion
- Type validation during deserialization
- Backwards compatibility support

Each type description has the format:
```
[TYPE_TAG][size][type_name_string][field1_string][field2_string]...
```

### 2. Data Section

Contains the actual serialized values in a linear, depth-first traversal order.

## Binary Tags

Each value in the binary format is preceded by a tag indicating its type:

| Tag   | Value | Type | Size | Description |
|-------|-------|------------|------|-------------|
| `'t'` | 0x74  | TYPE_TAG   | Variable | Type description with field names |
| `'b'` | 0x62  | BLOCK_TAG  | Variable | Block/struct container |
| `'s'` | 0x73  | STRING_TAG | Variable | Null-terminated string |
| `'f'` | 0x66  | FLOAT_TAG  | 5 bytes  | 32-bit IEEE 754 float |
| `'i'` | 0x69  | SMALL_INT_TAG | 2 bytes | 8-bit signed integer (-128 to 127) |
| `'I'` | 0x49  | INT_TAG    | 5 bytes | 32-bit signed integer |
| `'a'` | 0x61  | ARRAY_TAG  | Variable | Array with element count |

## Data Encoding

### Headers

Variable-size objects (blocks, arrays, strings, types) use a common header format:
```
[tag][size_byte0][size_byte1][size_byte2]
```
- `tag`: 1 byte type identifier
- `size`: 3 bytes (24-bit) little-endian size of entire object including header

### Integer Encoding

**Small Integer (SMALL_INT_TAG)**
- Range: -128 to 127
- Format: `[tag][value]`
- Size: 2 bytes total

**Regular Integer (INT_TAG)**
- Range: -2,147,483,648 to 2,147,483,647
- Format: `[tag][byte0][byte1][byte2][byte3]`
- Size: 5 bytes total
- Encoding: Little-endian

### Float Encoding

**Float (FLOAT_TAG)**
- Type: 32-bit IEEE 754 single precision
- Format: `[tag][byte0][byte1][byte2][byte3]`
- Size: 5 bytes total
- Encoding: Little-endian (IEEE 754 binary32)

### String Encoding

**String (STRING_TAG)**
- Format: `[tag][size0][size1][size2][characters...][null]`
- Size includes header (4 bytes) + string data + null terminator
- Character encoding: UTF-8
- Always null-terminated

Example: "hello"
```
0x73 0x0B 0x00 0x00 0x68 0x65 0x6C 0x6C 0x6F 0x00
 's' [size=11]       'h'  'e'  'l'  'l'  'o'  \0
```

### Block/Struct Encoding

**Block (BLOCK_TAG)**
- Format: `[tag][size0][size1][size2][field_values...]`
- Fields are serialized in declaration order
- No field names in data section (names are in type descriptions)

Example struct:
```vult
type Filter {
    cutoff : real;
    resonance : real;
}
```

Binary encoding:
```
0x62 [size] [cutoff_value] [resonance_value]
```

### Array Encoding

**Array (ARRAY_TAG)**
- Format: `[tag][size0][size1][size2][element_count][elements...]`
- `element_count`: Serialized as an integer value
- Elements follow immediately after count

Example: `[1.0, 2.0, 3.0]`
```
0x61 [size] [count=3] [1.0] [2.0] [3.0]
```

## Type Descriptions

Type descriptions provide metadata for proper deserialization and JSON conversion.

**Type Description (TYPE_TAG)**
- Format: `[tag][size][type_name_string][field1_string][field2_string]...`
- Appears only in type description section
- One description per unique struct type

Example for the Filter type above:
```
0x74 [size] [string:"Filter"] [string:"cutoff"] [string:"resonance"]
```

## Serialization Process

### Writing

1. **Collect Type Information**
   - Scan for all struct types marked with `@[save]`
   - Build type registry with field names

2. **Write Type Descriptions**
   - Output TYPE_TAG entries for each struct type
   - Include type name and ordered field names

3. **Write Data**
   - Traverse data structure depth-first
   - Write appropriate tag and value for each field
   - Use smallest integer representation when possible

### Reading

1. **Parse Type Descriptions**
   - Build type registry from TYPE_TAG entries
   - Stop when first BLOCK_TAG encountered

2. **Deserialize Data**
   - Read tag to determine type
   - Read appropriate number of bytes based on tag
   - Use type registry to map fields for structs

## Memory Layout Example

Consider this Vult code:
```vult
type Oscillator {
    frequency : real;
    amplitude : real;
}

fun process() {
    mem osc : Oscillator @[save];
    mem gain : real @[save];
    mem enabled : int @[save];
}
```

With values:
- `osc.frequency = 440.0`
- `osc.amplitude = 0.8`
- `gain = 0.5`
- `enabled = 1`

Binary representation:
```
# Type Descriptions Section
0x74 0x1F 0x00 0x00  # TYPE_TAG, size=31
  0x73 0x0F 0x00 0x00 "Oscillator" 0x00  # Type name
  0x73 0x0E 0x00 0x00 "frequency" 0x00   # Field 1
  0x73 0x0E 0x00 0x00 "amplitude" 0x00   # Field 2

# Data Section
0x62 0x0E 0x00 0x00           # BLOCK_TAG, size=14 (main block)
  0x62 0x0A 0x00 0x00         # BLOCK_TAG, size=10 (osc struct)
    0x66 0x00 0x00 0xDC 0x43  # FLOAT_TAG, 440.0
    0x66 0xCD 0xCC 0x4C 0x3F  # FLOAT_TAG, 0.8
  0x66 0x00 0x00 0x00 0x3F    # FLOAT_TAG, 0.5 (gain)
  0x69 0x01                   # SMALL_INT_TAG, 1 (enabled)
```

## CustomBuffer Structure

The serialization uses a `CustomBuffer` type defined in `vultin.hpp`:

```cpp
struct CustomBuffer {
    std::vector<uint8_t> data;
    bool error;
    bool calculate_size;
};
```

## Helper Functions

Key functions from `vultin.hpp` and `vultin.cpp`:

### Writing
- `push_byte(buffer, value)` - Add single byte
- `push_int(buffer, index, value)` - Add integer (auto-selects size)
- `push_float(buffer, index, value)` - Add float
- `push_string(buffer, index, str)` - Add string
- `push_block_header(buffer, index)` - Start block
- `push_array(buffer, index, count)` - Start array
- `update_size(buffer, start, size)` - Update block size

### Reading
- `read_byte(buffer, index)` - Read single byte
- `deserialize_int(buffer, index)` - Read integer
- `deserialize_float(buffer, index)` - Read float
- `deserialize_string(buffer, index)` - Read string
- `block_size(buffer, index)` - Get block size
- `next_object(buffer, index)` - Skip to next value

## Integration with Vult

### Code Generation

The Vult compiler generates serialization code for types marked with `@[save]`:

```cpp
// Generated serializer
int32_t MyType_serialize_data(CustomBuffer& buffer, int32_t index, const MyType& data) {
    int32_t start = index;
    index = push_block_header(buffer, index);
    index = push_float(buffer, index, data.field1);
    index = push_int(buffer, index, data.field2);
    update_size(buffer, start, index - start);
    return index;
}

// Generated deserializer
void MyType_deserialize_data(CustomBuffer& buffer, CustomTypeDescr& descr,
                            int32_t index, MyType& data) {
    int32_t field_index = search_field_name(buffer, descr, index, "field1");
    if (field_index >= 0) {
        data.field1 = deserialize_float(buffer, field_index);
    }
    // ... more fields
}
```

### Usage Pattern

```cpp
// Serialize
CustomBuffer buffer;
serialize_data(buffer, MyType_serialize_type_descr,
               MyType_serialize_data, my_instance);

// Deserialize
MyType restored;
deserialize_data(buffer, MyType_deserialize_data,
                 "MyType", restored);
```

## JSON Conversion

The binary format can be converted to/from JSON for:
- Human-readable configuration files
- Preset management
- Debugging and inspection
- Web API integration

See `json_serializer.hpp` and `json_serializer_jansson.hpp` for conversion implementations.

## Design Rationale

### Type Safety
- Tagged format prevents type confusion
- Type descriptions enable validation
- Field names preserved for round-trip accuracy

### Performance
- Linear memory layout for cache efficiency
- Minimal overhead (1-5 bytes per value)
- Direct memory access patterns
- No dynamic allocation during serialization

### Flexibility
- Variable-size encoding for space efficiency
- Extensible via new tags
- Platform-independent byte order
- Forward compatibility through type descriptions

## Limitations

- Maximum object size: 16MB (24-bit size field)
- No circular references
- No polymorphic types
- Limited to Vult's type system

## Version History

- v1.0: Initial format specification
- Current: Stable, production-ready format

---

This format provides an efficient, type-safe serialization mechanism optimized for audio DSP applications while maintaining flexibility for configuration and preset management through JSON conversion.