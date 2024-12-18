if (EC_INIT_DONE LESS 2)
    include(CTest)
    add_custom_target(check COMMAND CTEST_OUTPUT_ON_FAILURE=true ${CMAKE_CTEST_COMMAND})

    if(BUILD_TESTING)

       message(STATUS "(EC) Generating burp tests")

        set (TESTS read1 read2 like_liburp write1 write2 write2f write3 write3f write_update)
        foreach(test ${TESTS})
            add_executable(${test} ${test}.f90)
            target_link_libraries(${test} burp::burp)
            add_test(NAME ${test} COMMAND ${CMAKE_CURRENT_BINARY_DIR}/${test})
            add_dependencies(check ${test})
        endforeach()

    endif()
endif()
