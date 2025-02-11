set(PROJECT_C_FILES burp_api.c)
set(PROJECT_INCLUDE_FILES burp_api.h declare.h)

add_library(burpc-static STATIC ${PROJECT_C_FILES})
target_link_libraries(burpc-static PRIVATE rmn::rmn) 
target_include_directories(burpc-static PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>)
target_include_directories(burpc-static INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>)

add_dependencies(burpc-static ${PROJECT_NAME}_build_info)

add_library(burpc-shared SHARED $<TARGET_OBJECTS:burpc-static>)
target_include_directories(burpc-shared PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>)
target_include_directories(burpc-shared INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>)
                        
set_target_properties(burpc-static burpc-shared PROPERTIES
   VERSION ${LIBBURPC_VERSION}
   PUBLIC_HEADER "${PROJECT_INCLUDE_FILES}"
   INTERFACE_COMPILE_DEFINITIONS HAVE_BURPC="${LIBBURPC_VERSION}"
   OUTPUT_NAME burpc
   POSITION_INDEPENDENT_CODE ON)

add_library(burp::burpc-static ALIAS burpc-static)
add_library(burp::burpc-shared ALIAS burpc-shared)
add_library(burp::burpc ALIAS burpc-static) # Default to static
   
install(TARGETS burpc-static
    EXPORT ${PROJECT_NAME}-static-targets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    INCLUDES DESTINATION include
    PUBLIC_HEADER DESTINATION include)  

install(TARGETS burpc-shared
    EXPORT ${PROJECT_NAME}-shared-targets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    INCLUDES DESTINATION include
    PUBLIC_HEADER DESTINATION include)     

ec_install_symlink(./libburpc.a ${CMAKE_INSTALL_PREFIX}/lib/libburp_c.a)
ec_install_symlink(./libburpc.so ${CMAKE_INSTALL_PREFIX}/lib/libburp_c.so)
ec_install_symlink(./libburpc.so.${LIBBURPC_VERSION} ${CMAKE_INSTALL_PREFIX}/lib/libburp_c.so.${LIBBURPC_VERSION})
