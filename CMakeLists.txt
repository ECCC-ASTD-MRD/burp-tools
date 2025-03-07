# The minimum version can not be 3.16 because of the LINK_LANGUAGE generator expression in librmn's rmnConfig.cmake
cmake_minimum_required(VERSION 3.20)

#----- Append EC specific module path
list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake_rpn/modules $ENV{EC_CMAKE_MODULE_PATH})

include(ec_init)           # Initialise compilers and ec specific functions
ec_git_version()           # Get version from git state
ec_parse_manifest()        # Parse MANIFEST file

project(${NAME} DESCRIPTION "${DESCRIPTION}" LANGUAGES C CXX Fortran)
set(PROJECT_VERSION ${VERSION}${STATE} CACHE STRING "Define the version of burp-tools that will be baked into the various artifacts of this build" FORCE)

include(GNUInstallDirs)
set(CMAKE_INSTALL_LIBDIR lib CACHE INTERNAL "Install dir for libs" FORCE)  # Avoid using lib64, cache when in super-project

include(ec_compiler_presets)

set(CMAKE_VERBOSE_MAKEFILE TRUE)
set(CMAKE_C_STANDARD 99)
set(CMAKE_Fortran_PREPROCESS ON)

ec_build_info()            # Generate build information

if (NOT rmn_FOUND)
   find_package(rmn ${rmn_REQ_VERSION} REQUIRED)
endif()

find_package(IDL ${IDL_REQ_VERSION})
if (IDL_FOUND)
   add_subdirectory(src/libburpidl)
endif()

# Generate the config file for the project to be usable via cmake's find_package command
set(CONFIG_INSTALL_DIR "${CMAKE_INSTALL_LIBDIR}/cmake/burp-${PROJECT_VERSION}")

include(CMakePackageConfigHelpers)
configure_package_config_file(
    "Config.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/burpConfig.cmake"
    INSTALL_DESTINATION "${CONFIG_INSTALL_DIR}"
    PATH_VARS CMAKE_INSTALL_INCLUDEDIR CMAKE_INSTALL_LIBDIR
)
write_basic_package_version_file(
    "${CMAKE_CURRENT_BINARY_DIR}/burpConfigVersion.cmake"
    COMPATIBILITY SameMajorVersion
)
install(
    FILES
        "${CMAKE_CURRENT_BINARY_DIR}/burpConfig.cmake"
        "${CMAKE_CURRENT_BINARY_DIR}/burpConfigVersion.cmake"
    DESTINATION "${CONFIG_INSTALL_DIR}"
)

add_subdirectory(src/libburp)
add_subdirectory(src/libburpc)
add_subdirectory(src/editbrp)
add_subdirectory(src/brpvoir)
add_subdirectory(test)

install(TARGETS burp-static burpc-static
   EXPORT burp-static-targets
   LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
   ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
   INCLUDES DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
   PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})  

install(TARGETS burp-shared burpc-shared
   EXPORT burp-shared-targets
   LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
   ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
   INCLUDES DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
   PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})     

install(EXPORT burp-static-targets
   NAMESPACE burp::
   DESTINATION ${CONFIG_INSTALL_DIR})

install(EXPORT burp-shared-targets
   NAMESPACE burp::
   DESTINATION ${CONFIG_INSTALL_DIR})
   
#----- Packaging
ec_package_name()                # Define package prefix  
ec_build_config()                # Create build configuration script
ec_prepare_ssm()                 # Prepare ssm packaging files

set(CPACK_GENERATOR "TGZ")
set(CPACK_PACKAGE_VENDOR "ECCC")
set(CPACK_PACKAGE_CONTACT "${MAINTAINER}")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE.txt")
set(CPACK_RESOURCE_FILE_README "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
set(CPACK_OUTPUT_FILE_PREFIX "${CMAKE_CURRENT_BINARY_DIR}/package")
set(CPACK_PACKAGE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
set(CPACK_PACKAGE_FILE_NAME "${PACKAGE_NAME}")
set(CPACK_SOURCE_PACKAGE_FILE_NAME "${NAME}_${PROJECT_VERSION}")
include(CPack)
