
cmake_minimum_required(VERSION 2.8s)

# Gets the location of the ocaml libraries
execute_process(
   COMMAND ocamlfind query stdlib
   OUTPUT_VARIABLE OCAMLLIB_STR)
string(STRIP ${OCAMLLIB_STR} OCAMLLIB)

link_directories(${OCAMLLIB})
include_directories(${OCAMLLIB})

# Defines the object extension acording to the platform
if(WIN32)
   set(OBJ_EXT obj)
   set(LIBRARIES libasmrun ws2_32)
endif(WIN32)

if(UNIX AND NOT APPLE)
   set(OBJ_EXT o)
   set(LIBRARIES asmrun_pic m dl)
endif(UNIX AND NOT APPLE)

if(APPLE)
   set(OBJ_EXT o)
   set(LIBRARIES asmrun_pic)
endif(APPLE)

# Defines the path of the output object
set(OBJECT_OUTPUT ${CMAKE_CURRENT_LIST_DIR}/../_build/src/driver.native.${OBJ_EXT})

# Gets all the ml files
file(GLOB_RECURSE VULT_CODE
   "${CMAKE_CURRENT_LIST_DIR}/../src/*.ml"
)

# Calls ocamlbuild to build the object
add_custom_command(
   OUTPUT ${OBJECT_OUTPUT}
   COMMAND ocamlbuild -use-ocamlfind -no-hygiene -pkg pla -pkg containers -pkg ppx_deriving.std src/driver.native.${OBJ_EXT}
   DEPENDS ${VULT_CODE}
   WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}/../ )
add_custom_target(vult_object ALL DEPENDS ${OBJECT_OUTPUT})

# Creates the shared library
add_library(vult SHARED exports.c flexdll_stub.c ${OBJECT_OUTPUT})
target_link_libraries(vult ${LIBRARIES})

# Creates an executable to test the functions
add_executable(vtest vtest.c)

target_link_libraries(vtest vult)
