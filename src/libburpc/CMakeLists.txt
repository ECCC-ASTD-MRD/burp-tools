set(PROJECT_C_FILES burp_api.c)
set(PROJECT_INCLUDE_FILES ${CMAKE_CURRENT_SOURCE_DIR}/burp_api.h ${CMAKE_CURRENT_SOURCE_DIR}/declare.h)

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

ec_install_symlink(./libburpc.a ${CMAKE_CURRENT_BINARY_DIR}/libburp_c.a)
ec_install_symlink(./libburpc.so ${CMAKE_CURRENT_BINARY_DIR}/libburp_c.so)
ec_install_symlink(./libburpc.so ${CMAKE_CURRENT_BINARY_DIR}/libburp_c_shared.so)
ec_install_symlink(./libburpc.so.${LIBBURPC_VERSION} ${CMAKE_CURRENT_BINARY_DIR}/libburp_c.so.${LIBBURPC_VERSION})
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/libburp_c.a 
   ${CMAKE_CURRENT_BINARY_DIR}/libburp_c.so
   ${CMAKE_CURRENT_BINARY_DIR}/libburp_c.so.${LIBBURPC_VERSION}
   ${CMAKE_CURRENT_BINARY_DIR}/libburp_c_shared.so
TYPE LIB) 
