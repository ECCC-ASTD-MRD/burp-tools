message(STATUS "(EC) Generating burp tests")

set (TESTS read1 read2 like_liburp like_liburp2 write1 write1f write2 write2f write3 write3f write_update prog_test)
foreach(test ${TESTS})
    add_executable(${test}f ${test}.f90)
    target_link_libraries(${test}f burp::burp rmn::rmn)
    add_test(NAME ${test}f COMMAND ${CMAKE_CURRENT_BINARY_DIR}/${test}f ${brpfile})
    add_dependencies(check ${test}f)
endforeach()
