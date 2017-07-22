
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

set(VULT vultc)

function(vult_pd output source includes)
   set(OUTPUT_FILES ${CMAKE_CURRENT_BINARY_DIR}/${output}.cpp ${CMAKE_CURRENT_BINARY_DIR}/${output}.h)

   foreach(dir ${${includes}})
      set(includes_flag -i ${dir} ${includes_flag})
   endforeach(dir)

   # check the file
    execute_process(
      COMMAND ${VULT} ${includes_flag} -check ${source}
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

   # get the dependencies
   execute_process(
      COMMAND ${VULT} ${includes_flag} -deps ${source}
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
      OUTPUT_VARIABLE DEPENDENCIES_STRING)

   if(DEPENDENCIES_STRING STREQUAL "")
      set(DEPENDENCIES)
   else()
      string(REPLACE " " ";" DEPENDENCIES ${DEPENDENCIES_STRING})
   endif()

   add_custom_command(
      OUTPUT ${OUTPUT_FILES}
      DEPENDS ${source} ${DEPENDENCIES}
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
      COMMAND ${VULT} ${includes_flag} ${source} -ccode -template pd -o ${CMAKE_CURRENT_BINARY_DIR}/${output})
   add_custom_target(${output}_code ALL DEPENDS ${output}.cpp ${output}.h)
   set(src ${OUTPUT_FILES} ${CMAKE_CURRENT_SOURCE_DIR}/../runtime/vultin.c ${CMAKE_CURRENT_SOURCE_DIR}/../runtime/vultin.h)
   add_pd_object(${output} src)
endfunction(vult_pd)