set(PROJECT_F90_FILES burp_module.f90)
set(MODULE_DIR ${CMAKE_CURRENT_BINARY_DIR}/include)

add_library(burp-static STATIC ${PROJECT_F90_FILES})
target_link_libraries(burp-static PRIVATE rmn::rmn) 

add_dependencies(burp-static ${PROJECT_NAME}_build_info)

add_library(burp-shared SHARED $<TARGET_OBJECTS:burp-static>)
target_link_libraries(burp-shared PRIVATE rmn::rmn-shared) 
                        
set_target_properties(burp-static burp-shared PROPERTIES
   VERSION ${LIBBURP_VERSION}
   Fortran_MODULE_DIRECTORY ${MODULE_DIR}
   PUBLIC_HEADER "${PROJECT_INCLUDE_FILES}"
   INTERFACE_COMPILE_DEFINITIONS HAVE_BURPF="${LIBBURP_VERSION}"
   INTERFACE_INCLUDE_DIRECTORIES $<BUILD_INTERFACE:${MODULE_DIR}>
   OUTPUT_NAME burpmodule
   POSITION_INDEPENDENT_CODE ON)

add_library(burp::burp-static ALIAS burp-static)
add_library(burp::burp-shared ALIAS burp-shared)
add_library(burp::burp ALIAS burp-static) # Default to static

install(FILES
    ${MODULE_DIR}/burp_block_class.mod
    ${MODULE_DIR}/burp_btyp_runn_flgs.mod
    ${MODULE_DIR}/burp_constants.mod
    ${MODULE_DIR}/burp_file_class.mod
    ${MODULE_DIR}/burp_module.mod
    ${MODULE_DIR}/burp_rpt_class.mod
    ${MODULE_DIR}/conversion.mod
    ${MODULE_DIR}/conversion_module.mod
    ${MODULE_DIR}/conversion_to_string.mod
    ${MODULE_DIR}/decimal_digits.mod
    ${MODULE_DIR}/errormessages.mod
    ${MODULE_DIR}/librmn_declaration.mod
    TYPE INCLUDE)


ec_install_symlink(./libburpmodule.a ${CMAKE_CURRENT_BINARY_DIR}/libburp_module.a)
ec_install_symlink(./libburpmodule.so ${CMAKE_CURRENT_BINARY_DIR}/libburp_module.so)
ec_install_symlink(./libburpmodule.so.${LIBBURP_VERSION} ${CMAKE_CURRENT_BINARY_DIR}/libburp_module.so.${LIBBURP_VERSION})
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/libburp_module.a 
    ${CMAKE_CURRENT_BINARY_DIR}/libburp_module.so
    ${CMAKE_CURRENT_BINARY_DIR}/libburp_module.so.${LIBBURP_VERSION}
TYPE LIB) 
