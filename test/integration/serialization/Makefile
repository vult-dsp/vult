VULT = ../../../_build/default/src/vult.exe
CXX = g++
CXXFLAGS = -std=c++11 -O0 -Wall

.PHONY: all clean test

all: test_serialization

# Generate C++ from Vult; the compiler also emits the vultin runtime files
test_serialization.cpp test_serialization.h vultin.cpp vultin.hpp: test_serialization.vult $(VULT)
	$(VULT) test_serialization.vult -code cpp -o test_serialization

# Compile the emitted runtime
vultin.o: vultin.cpp vultin.hpp
	$(CXX) $(CXXFLAGS) -c vultin.cpp -o vultin.o

# Compile generated code
test_serialization.o: test_serialization.cpp test_serialization.h
	$(CXX) $(CXXFLAGS) -c test_serialization.cpp -o test_serialization.o

# Compile test harness
test_serialization_main.o: test_serialization_main.cpp test_serialization.h
	$(CXX) $(CXXFLAGS) -c test_serialization_main.cpp -o test_serialization_main.o

# Link test executable
test_serialization: test_serialization.o test_serialization_main.o vultin.o
	$(CXX) $(CXXFLAGS) -o test_serialization test_serialization.o test_serialization_main.o vultin.o

# Run the test
test: test_serialization
	./test_serialization

clean:
	rm -f *.o test_serialization test_serialization.cpp test_serialization.h test_serialization.tables.h vultin.cpp vultin.hpp
