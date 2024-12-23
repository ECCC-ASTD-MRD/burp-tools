message(STATUS "(EC) Generating burpidl tests")

add_test(NAME IDL COMMAND test.sh WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
