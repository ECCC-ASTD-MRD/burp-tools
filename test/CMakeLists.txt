if (EC_INIT_DONE LESS 2)
    include(CTest)
    enable_testing()
    add_custom_target(check COMMAND CTEST_OUTPUT_ON_FAILURE=true ${CMAKE_CTEST_COMMAND})
    add_custom_target(test_execs)
    add_dependencies(check test_execs)

    if(BUILD_TESTING)
       set(brpfile ${CMAKE_CURRENT_SOURCE_DIR}/data/2016072212_)

        if (IDL_FOUND)
            # Requires r.filetype
            #add_subdirectory(libburpidl)
        endif()
        
        add_subdirectory(libburp)
        add_subdirectory(libburpc)
 
    endif()
endif()
