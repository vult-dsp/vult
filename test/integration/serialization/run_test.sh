#!/bin/bash
set -e

echo "=== Serialization Integration Test ==="
echo ""

# Build Vult compiler first
echo "Step 1: Building Vult compiler..."
(cd ../../.. && dune build)
echo "Done."
echo ""

# Clean and build the test
echo "Step 2: Generating C++ code and compiling..."
make clean
make all
echo "Done."
echo ""

# Run the test
echo "Step 3: Running serialization test..."
echo ""
./test_serialization
echo ""

echo "=== Test Complete ==="
