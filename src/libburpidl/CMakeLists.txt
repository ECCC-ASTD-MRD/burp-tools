set(PROJECT_C_FILES libburp_idl.c)

add_library(burpidl SHARED ${PROJECT_C_FILES})
target_link_libraries(burpidl PRIVATE rmn::rmn ${IDL_LIBRARIES}) 
target_include_directories(burpidl PRIVATE ${IDL_INCLUDES})

add_dependencies(burpidl ${PROJECT_NAME}_build_info)

set_target_properties(burpidl PROPERTIES
   VERSION ${LIBBURPIDL_VERSION}
   INTERFACE_COMPILE_DEFINITIONS HAVE_BURPIDL="${LIBBURPIDL_VERSION}"
   OUTPUT_NAME burpidl
   POSITION_INDEPENDENT_CODE ON)

add_library(burp::burpidl ALIAS burpidl)
   
install(TARGETS burpidl
    EXPORT ${PROJECT_NAME}-shared-targets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    INCLUDES DESTINATION include
    PUBLIC_HEADER DESTINATION include)      
 
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/../../share/scripts
    DESTINATION share)