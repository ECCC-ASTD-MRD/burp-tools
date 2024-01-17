message(STATUS "Generating tests target for libburp_c")

message(STATUS "CMAKE_Fortran_COMPILER=${CMAKE_Fortran_COMPILER}")
message(STATUS "CMAKE_Fortran_SOURCE_FILE_EXTENSIONS=${CMAKE_Fortran_SOURCE_FILE_EXTENSIONS}")

include(CTest)

set(TARGET_SOURCE_FILES read1.c readcc.cpp readburp.c readfloat.c setit.cpp write1.cpp write2.cpp write2f.cpp maxlen.cpp obs.cpp val.cpp elements.cpp)

foreach(ITEM ${TARGET_SOURCE_FILES})
   message(STATUS "ITEM = ${ITEM}")
   string(REGEX MATCH "(.+)\\.(.+)" null ${ITEM})
   if (NOT ${CMAKE_MATCH_1} STREQUAL "")
      set(TARGET_BINARY ${CMAKE_MATCH_1})
      add_executable(${TARGET_BINARY} ${ITEM})
      if (${CMAKE_MATCH_2} STREQUAL "c")
	 cmds_che_add_main(${TARGET_BINARY} "my_main" Fortran)
      endif()
      if (${CMAKE_MATCH_2} STREQUAL "cpp")
         cmds_che_add_main(${TARGET_BINARY} "my_main" C)
      endif()
      target_include_directories(${TARGET_BINARY} PRIVATE ${CMAKE_BINARY_DIR})
      target_link_libraries(${TARGET_BINARY} PRIVATE burp_c rmn imf)
   endif()
endforeach()

set(binaries read1 setit maxlen obs readcc write2 write2f)

foreach(item ${binaries})
  add_test(NAME test_${item} COMMAND run_test.sh ${item})
endforeach()

# taking too long
#add_test(NAME test_elements COMMAND run_test.sh elements "-f")
add_test(NAME test_val COMMAND run_test.sh val "-f")
add_test(NAME test_write1 COMMAND run_test.sh write1 "" "./junk_output")

configure_file("${CMAKE_CURRENT_SOURCE_DIR}/run_test.sh" "${CMAKE_CURRENT_BINARY_DIR}" @ONLY)
