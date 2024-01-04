# get current CMakeLists.txt directory
get_filename_component(_CURRENT_DIR "${CMAKE_CURRENT_LIST_FILE}" PATH)
message(STATUS "Generating targets in: ${_CURRENT_DIR}")


set(TARGET_SOURCE_FILES burp_api.c)
set(STATIC_LIB burp_c)
set(SHARED_LIB burp_c_shared)

add_library(${STATIC_LIB} STATIC ${TARGET_SOURCE_FILES})
add_library(${SHARED_LIB} SHARED ${TARGET_SOURCE_FILES})

target_include_directories(${STATIC_LIB} INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories(${SHARED_LIB} INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})


install(TARGETS ${STATIC_LIB} ${SHARED_LIB} DESTINATION lib)
install( FILES ${CMAKE_CURRENT_SOURCE_DIR}/burp_api.h DESTINATION include)
install( FILES ${CMAKE_CURRENT_SOURCE_DIR}/declare.h DESTINATION include)
