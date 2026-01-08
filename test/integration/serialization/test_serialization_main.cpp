#include <iostream>
#include <cmath>
#include <cstring>
#include "test_serialization.h"
#include "vultin.hpp"

// Helper to compare floats
bool floatEqual(float a, float b, float epsilon = 0.0001f) {
    return std::fabs(a - b) < epsilon;
}

// Helper to verify a string field
bool verifyString(const std::string& name, const std::string& original, const std::string& restored, int& errors) {
    if (original != restored) {
        std::cerr << "FAIL: " << name << " mismatch: '" << original << "' (len=" << original.length()
                  << ") != '" << restored << "' (len=" << restored.length() << ")" << std::endl;
        errors++;
        return false;
    }
    return true;
}

// Number of type descriptions (will be determined after code generation)
#define NUM_TYPE_DESCR 4

int main() {
    std::cout << "=== Serialization Integration Test ===" << std::endl;

    // Create and initialize original instance
    // The @[init] function setTestValues() is called automatically by _init
    Test_serialization_main_type original;
    Test_serialization_main_type_init(original);

    std::cout << "Original values set via @[init] function:" << std::endl;
    std::cout << "  int_val = " << original.int_val << std::endl;
    std::cout << "  real_val = " << original.real_val << std::endl;
    std::cout << "  bool_val = " << (original.bool_val ? "true" : "false") << std::endl;
    std::cout << "  str_val = \"" << original.str_val << "\"" << std::endl;
    std::cout << "  pt = (" << original.pt.x << ", " << original.pt.y << ")" << std::endl;
    std::cout << "  int_list size = " << original.int_list.size() << std::endl;
    std::cout << "  pt_list size = " << original.pt_list.size() << std::endl;

    // Serialize
    CustomBuffer buffer;
    serialize_data<NUM_TYPE_DESCR>(buffer,
        Test_serialization_main_type_serialize_type_descr,
        Test_serialization_main_type_serialize_data,
        original);

    std::cout << std::endl << "Serialized " << buffer.data.size() << " bytes" << std::endl;

    // Create new instance for deserialization (will have default values initially)
    Test_serialization_main_type restored;
    Test_serialization_main_type_init(restored);

    // Clear restored values to ensure we're testing deserialization properly
    restored.int_list.clear();
    restored.pt_list.clear();
    restored.int_val = 0;
    restored.real_val = 0.0f;
    restored.bool_val = false;
    restored.str_val = "";
    // Clear all string fields being tested for padding
    restored.str_empty = "X";  // Set to non-empty to verify empty string works
    restored.str_1 = "";
    restored.str_2 = "";
    restored.str_3 = "";
    restored.str_4 = "";
    restored.str_5 = "";
    restored.str_7 = "";
    restored.str_8 = "";
    restored.str_15 = "";
    restored.str_16 = "";
    restored.str_31 = "";
    restored.str_32 = "";

    // Deserialize
    deserialize_data(buffer,
        Test_serialization_main_type_deserialize_data,
        "Test_serialization_main_type",
        restored);

    std::cout << "Deserialized successfully" << std::endl << std::endl;

    // Verify all values
    int errors = 0;

    std::cout << "Verifying values..." << std::endl;

    // Verify primitives
    if (original.int_val != restored.int_val) {
        std::cerr << "FAIL: int_val mismatch: " << original.int_val << " != " << restored.int_val << std::endl;
        errors++;
    } else {
        std::cout << "  [OK] int_val" << std::endl;
    }

    if (!floatEqual(original.real_val, restored.real_val)) {
        std::cerr << "FAIL: real_val mismatch: " << original.real_val << " != " << restored.real_val << std::endl;
        errors++;
    } else {
        std::cout << "  [OK] real_val" << std::endl;
    }

    if (original.bool_val != restored.bool_val) {
        std::cerr << "FAIL: bool_val mismatch" << std::endl;
        errors++;
    } else {
        std::cout << "  [OK] bool_val" << std::endl;
    }

    if (original.str_val != restored.str_val) {
        std::cerr << "FAIL: str_val mismatch: '" << original.str_val << "' != '" << restored.str_val << "'" << std::endl;
        errors++;
    } else {
        std::cout << "  [OK] str_val" << std::endl;
    }

    // Verify strings of different sizes (padding test)
    std::cout << "  Testing string padding..." << std::endl;
    bool all_strings_ok = true;
    all_strings_ok &= verifyString("str_empty (0 chars)", original.str_empty, restored.str_empty, errors);
    all_strings_ok &= verifyString("str_1 (1 char)", original.str_1, restored.str_1, errors);
    all_strings_ok &= verifyString("str_2 (2 chars)", original.str_2, restored.str_2, errors);
    all_strings_ok &= verifyString("str_3 (3 chars)", original.str_3, restored.str_3, errors);
    all_strings_ok &= verifyString("str_4 (4 chars)", original.str_4, restored.str_4, errors);
    all_strings_ok &= verifyString("str_5 (5 chars)", original.str_5, restored.str_5, errors);
    all_strings_ok &= verifyString("str_7 (7 chars)", original.str_7, restored.str_7, errors);
    all_strings_ok &= verifyString("str_8 (8 chars)", original.str_8, restored.str_8, errors);
    all_strings_ok &= verifyString("str_15 (15 chars)", original.str_15, restored.str_15, errors);
    all_strings_ok &= verifyString("str_16 (16 chars)", original.str_16, restored.str_16, errors);
    all_strings_ok &= verifyString("str_31 (31 chars)", original.str_31, restored.str_31, errors);
    all_strings_ok &= verifyString("str_32 (32 chars)", original.str_32, restored.str_32, errors);
    if (all_strings_ok) {
        std::cout << "  [OK] all string sizes (0, 1, 2, 3, 4, 5, 7, 8, 15, 16, 31, 32 chars)" << std::endl;
    }

    // Verify struct
    if (!floatEqual(original.pt.x, restored.pt.x) ||
        !floatEqual(original.pt.y, restored.pt.y)) {
        std::cerr << "FAIL: point mismatch" << std::endl;
        errors++;
    } else {
        std::cout << "  [OK] point struct" << std::endl;
    }

    // Verify nested struct
    if (!floatEqual(original.rc.origin.x, restored.rc.origin.x) ||
        !floatEqual(original.rc.origin.y, restored.rc.origin.y) ||
        !floatEqual(original.rc.size.x, restored.rc.size.x) ||
        !floatEqual(original.rc.size.y, restored.rc.size.y)) {
        std::cerr << "FAIL: rect mismatch" << std::endl;
        errors++;
    } else {
        std::cout << "  [OK] rect (nested struct)" << std::endl;
    }

    // Verify int array
    bool int_arr_ok = true;
    for (int i = 0; i < 4; i++) {
        if (original.int_arr[i] != restored.int_arr[i]) {
            std::cerr << "FAIL: int_arr[" << i << "] mismatch: "
                      << original.int_arr[i] << " != " << restored.int_arr[i] << std::endl;
            errors++;
            int_arr_ok = false;
        }
    }
    if (int_arr_ok) {
        std::cout << "  [OK] int_arr (array of int)" << std::endl;
    }

    // Verify point array
    bool pt_arr_ok = true;
    for (int i = 0; i < 3; i++) {
        if (!floatEqual(original.pt_arr[i].x, restored.pt_arr[i].x) ||
            !floatEqual(original.pt_arr[i].y, restored.pt_arr[i].y)) {
            std::cerr << "FAIL: pt_arr[" << i << "] mismatch" << std::endl;
            errors++;
            pt_arr_ok = false;
        }
    }
    if (pt_arr_ok) {
        std::cout << "  [OK] pt_arr (array of struct)" << std::endl;
    }

    // Verify int list
    if (original.int_list.size() != restored.int_list.size()) {
        std::cerr << "FAIL: int_list size mismatch: "
                  << original.int_list.size() << " != " << restored.int_list.size() << std::endl;
        errors++;
    } else {
        bool int_list_ok = true;
        for (size_t i = 0; i < original.int_list.size(); i++) {
            if (original.int_list[i] != restored.int_list[i]) {
                std::cerr << "FAIL: int_list[" << i << "] mismatch: "
                          << original.int_list[i] << " != " << restored.int_list[i] << std::endl;
                errors++;
                int_list_ok = false;
            }
        }
        if (int_list_ok) {
            std::cout << "  [OK] int_list (list of int)" << std::endl;
        }
    }

    // Verify point list
    if (original.pt_list.size() != restored.pt_list.size()) {
        std::cerr << "FAIL: pt_list size mismatch: "
                  << original.pt_list.size() << " != " << restored.pt_list.size() << std::endl;
        errors++;
    } else {
        bool pt_list_ok = true;
        for (size_t i = 0; i < original.pt_list.size(); i++) {
            if (!floatEqual(original.pt_list[i].x, restored.pt_list[i].x) ||
                !floatEqual(original.pt_list[i].y, restored.pt_list[i].y)) {
                std::cerr << "FAIL: pt_list[" << i << "] mismatch" << std::endl;
                errors++;
                pt_list_ok = false;
            }
        }
        if (pt_list_ok) {
            std::cout << "  [OK] pt_list (list of struct)" << std::endl;
        }
    }

    // Summary
    std::cout << std::endl;
    if (errors == 0) {
        std::cout << "SUCCESS: All checks passed!" << std::endl;
        return 0;
    } else {
        std::cerr << "FAILED: " << errors << " errors" << std::endl;
        return 1;
    }
}
