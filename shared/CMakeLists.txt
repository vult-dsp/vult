 cmake_minimum_required(VERSION 2.8)

# Gets the location of the ocaml libraries
execute_process(
   COMMAND ocamlfind query stdlib
   OUTPUT_VARIABLE OCAMLLIB_STR)
string(STRIP ${OCAMLLIB_STR} OCAMLLIB)

link_directories(${OCAMLLIB})
include_directories(${OCAMLLIB})

if(WIN32)
   set(OBJ_EXT obj)
else(WIN32)
   set(OBJ_EXT o)
endif(WIN32)

set(OBJECT_OUTPUT ${CMAKE_CURRENT_LIST_DIR}/../_build/src/driver.native.${OBJ_EXT})

add_custom_command(
   OUTPUT ${OBJECT_OUTPUT}
   COMMAND ocamlbuild -use-ocamlfind -no-hygiene -pkg pla -pkg containers -pkg ppx_deriving.std src/driver.native.${OBJ_EXT}
   WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}/../
)

add_custom_target(vult_object ALL DEPENDS ${OBJECT_OUTPUT})

add_library(vult_shared SHARED exports.c ${OBJECT_OUTPUT})

target_link_libraries(vult_shared asmrun_pic)
