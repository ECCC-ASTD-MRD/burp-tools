set(FICHIERS_FTN
   barf.f 	   bdesire.f  	 bexdes.f  	 brpcopi.f 
   editbrp.f   fermbs.f  	 jhmenm.f  	 julm.f 
   opodate.f   ouvrebs.f  	 restdez.f   spool.f 
   jdatxx.f    prefix.f)


add_executable(editbrp ${FICHIERS_FTN})
target_link_libraries(editbrp PRIVATE rmn) 

install(TARGETS editbrp
    RUNTIME DESTINATION bin)