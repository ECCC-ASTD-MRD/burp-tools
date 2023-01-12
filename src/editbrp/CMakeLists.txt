set(FICHIERS_FTN
   barf.f90	     bdesire.f90  	 bexdes.f90  	 brpcopi.f90 
   editbrp.f90   fermbs.f90  	    jhmenm.f90  	 julm.f90 
   opodate.f90   ouvrebs.f90  	 restdez.f90    spool.f90 
   jdatxx.f90    prefix.f90)


add_executable(editbrp ${FICHIERS_FTN})
target_link_libraries(editbrp PRIVATE rmn) 

install(TARGETS editbrp
    RUNTIME DESTINATION bin)