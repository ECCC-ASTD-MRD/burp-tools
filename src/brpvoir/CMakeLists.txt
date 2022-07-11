set(FICHIERS_F brpvoir.f mrfvoi.f)
set(FICHIERS_C rai2char.c)

add_executable(brpvoir ${FICHIERS_C} ${FICHIERS_F})
target_link_libraries(brpvoir PRIVATE rmn) 

install(TARGETS brpvoir
    RUNTIME DESTINATION bin)