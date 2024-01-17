cmake_minimum_required(VERSION 3.16)

LIST(APPEND CMAKE_MODULE_PATH $ENV{CMAKE_MODULE_PATH})
message(STATUS "CMAKE_MODULE_PATH=${CMAKE_MODULE_PATH}")

set(USE_INTEL_NEXTGEN TRUE)

include(cmds_che_init)         # Initialize build and configs

cmds_che_init_build()

enable_language(C)

if(HAS_MANIFEST)
   project(${NAME} 
	   VERSION     ${VERSION}
	   DESCRIPTION "${DESCRIPTION}")
else()
   project("burp_c"
	   VERSION     ${VERSION}
	   DESCRIPTION "")
endif()

include(CTest)

message(STATUS "PROJECT_NAME=${PROJECT_NAME}")
message(STATUS "PROJECT_VERSION=${PROJECT_VERSION}")

enable_language(C Fortran)
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
message(STATUS "CMAKE_Fortran_FLAGS=${CMAKE_Fortran_FLAGS}")

if (NOT RMN_FOUND)
   find_package(rmn ${RMN_VERSION} REQUIRED COMPONENTS shared)
endif()

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/src)
add_subdirectory(src)
add_subdirectory(tests)

set(CHE_GROUP_NAME       "wxobs-libs")
set(CHE_STATIC_LIB_NAMES "burp_c")
set(CHE_SHARED_LIB_NAMES "burp_c_shared")

cmds_che_init_config()

set(LICENSE_FILENAME "COPYING.LESSER")
cmds_che_init_packaging()
