set(PROJECT_F90_FILES burp_module.f90)
set(MODULE_DIR ${CMAKE_CURRENT_BINARY_DIR}/include)

add_library(libburp-static STATIC ${PROJECT_F90_FILES})
target_link_libraries(libburp-static PRIVATE rmn::rmn) 

add_dependencies(libburp-static ${PROJECT_NAME}_build_info)

add_library(libburp-shared SHARED $<TARGET_OBJECTS:libburp-static>)
                        
set_target_properties(libburp-static libburp-shared PROPERTIES
   VERSION ${PROJECT_VERSION}
   Fortran_MODULE_DIRECTORY ${MODULE_DIR}
   PUBLIC_HEADER "${PROJECT_INCLUDE_FILES}"
   INTERFACE_COMPILE_DEFINITIONS HAVE_BURP="${PROJECT_VERSION}"
   INTERFACE_INCLUDE_DIRECTORIES $<BUILD_INTERFACE:${MODULE_DIR}>
   OUTPUT_NAME burpmodule
   POSITION_INDEPENDENT_CODE ON)

add_library(libburp::libburp-static ALIAS libburp-static)
add_library(libburp::libburp-shared ALIAS libburp-shared)
add_library(libburp::libburp ALIAS libburp-static) # Default to static
   
install(TARGETS libburp-static
    EXPORT ${PROJECT_NAME}-static-targets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    INCLUDES DESTINATION include
    PUBLIC_HEADER DESTINATION include)  

install(TARGETS libburp-shared
    EXPORT ${PROJECT_NAME}-shared-targets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    INCLUDES DESTINATION include
    PUBLIC_HEADER DESTINATION include)  

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
    