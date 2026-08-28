
add_definitions(-DPD)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/cmake/pd-deps)

if(APPLE)
   set(LIB_SUFFIX ".pd_darwin")
   set(CMAKE_SHARED_LINKER_FLAGS "-undefined dynamic_lookup ${CMAKE_SHARED_LINKER_FLAGS}")
   set(LINK_LIBRARIES)
endif(APPLE)

if(UNIX AND NOT APPLE)
   set(LIB_SUFFIX ".pd_linux")
   set(LINK_LIBRARIES m c)
endif(UNIX AND NOT APPLE)

if(WIN32)
   set(LIB_SUFFIX ".dll")
   set(LINK_LIBRARIES pd)
   link_directories(${CMAKE_CURRENT_SOURCE_DIR}/pd-deps)
endif(WIN32)

macro(add_pd_object name files)
   add_library(${name} SHARED ${${files}})
   set_target_properties(${name} PROPERTIES
      OUTPUT_NAME "${name}~"
      SUFFIX ${LIB_SUFFIX}
      PREFIX "")
   target_link_libraries(${name} ${LINK_LIBRARIES})
endmacro(add_pd_object)

find_program(VULT NAMES ./_build/default/src/vult.exe vult  HINTS ${CMAKE_CURRENT_LIST_DIR}/../../)
message(STATUS "Vult compiler found in ${VULT}")

function(vult_pd output source includes)
   # every target generates into its own directory: the compiler emits the runtime files
   # (vultin.hpp/vultin.cpp) next to the generated code
   set(GEN_DIR ${CMAKE_CURRENT_BINARY_DIR}/${output})
   set(OUTPUT_FILES ${GEN_DIR}/${output}.cpp ${GEN_DIR}/${output}.h ${GEN_DIR}/vultin.cpp ${GEN_DIR}/vultin.hpp)

   foreach(dir ${${includes}})
      set(includes_flag -i ${dir} ${includes_flag})
   endforeach(dir)

   # get the dependencies
   execute_process(
      COMMAND ${VULT} ${includes_flag} -deps ${source}
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
      OUTPUT_VARIABLE DEPENDENCIES_STRING)

   if(DEPENDENCIES_STRING STREQUAL "")
      set(DEPENDENCIES)
   else()
      string(STRIP ${DEPENDENCIES_STRING} DEPENDENCIES_STRING)
      string(REPLACE " " ";" DEPENDENCIES ${DEPENDENCIES_STRING})
   endif()

   set(DEPENDENCY_FILES)
   foreach(dep ${DEPENDENCIES})
      if(IS_ABSOLUTE ${dep})
         list(APPEND DEPENDENCY_FILES ${dep})
      else()
         list(APPEND DEPENDENCY_FILES ${CMAKE_CURRENT_SOURCE_DIR}/${dep})
      endif()
   endforeach(dep)

   add_custom_command(
      OUTPUT ${OUTPUT_FILES}
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${source} ${DEPENDENCY_FILES}
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
      COMMAND ${CMAKE_COMMAND} -E make_directory ${GEN_DIR}
      COMMAND ${VULT} ${includes_flag} ${source} -code cpp -template pd -o ${GEN_DIR}/${output})
   add_custom_target(${output}_code ALL DEPENDS ${OUTPUT_FILES})
   set(src ${GEN_DIR}/${output}.cpp ${GEN_DIR}/vultin.cpp)
   add_pd_object(${output} src)
   target_include_directories(${output} PRIVATE ${GEN_DIR})
endfunction(vult_pd)