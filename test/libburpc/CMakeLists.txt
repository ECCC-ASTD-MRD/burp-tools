message(STATUS "(EC) Generating burpc tests")

set(TESTS_C read1 readburp readfloat)
set(TESTS_CPP1 readcc setit write1 write2 write2f maxlen obs)
set(TESTS_CPP2 val elements)

foreach(test ${TESTS_C})
   add_executable(${test}c ${test}.c)
   target_link_libraries(${test}c burp::burpc rmn::rmn)
   add_test(NAME ${test}c COMMAND ${CMAKE_CURRENT_BINARY_DIR}/${test}c ${brpfile})
   add_dependencies(check ${test}c)
endforeach()

foreach(test ${TESTS_CPP1})
   add_executable(${test}c ${test}.cpp)
   target_link_libraries(${test}c burp::burpc rmn::rmn)
   add_test(NAME ${test} COMMAND ${CMAKE_CURRENT_BINARY_DIR}/${test}c ${brpfile})
   add_dependencies(check ${test}c)
endforeach()

# Longer tests
#foreach(test ${TESTS_CPP2})
#   add_executable(${test}c ${test}.cpp)
#   target_link_libraries(${test}c burp::burpc rmn::rmn)
#   add_test(NAME ${test}c COMMAND ${CMAKE_CURRENT_BINARY_DIR}/${test}c -f ${brpfile})
#   add_dependencies(check ${test}c)
#endforeach()
