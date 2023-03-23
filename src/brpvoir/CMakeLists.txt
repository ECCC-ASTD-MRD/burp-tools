set(FICHIERS_F brpvoir.f)

add_executable(brpvoir ${FICHIERS_F})
target_link_libraries(brpvoir PRIVATE rmn::rmn) 

install(TARGETS brpvoir
    RUNTIME DESTINATION bin)