SHELL=/bin/sh


# The following macros establish general defaults. They are overridden
# in the "all" rule as needed to satisfy a given platform's requirements.



all: 
		echo " +++++++++++++++++++++++++++++++++"	
	        r.compile -src errormessages.cdk90 -arch SX5 
		echo " +++++++++++++++++++++++++++++++++"	
	        r.compile -src librmn_declaration.cdk90 -arch SX5 
		echo " +++++++++++++++++++++++++++++++++"	
	        r.compile -src object_initialization.cdk90 -arch SX5 
		echo " +++++++++++++++++++++++++++++++++"	
	        r.compile -src burp_block_class.cdk90 -arch SX5 
		echo " +++++++++++++++++++++++++++++++++"	
	        r.compile -src burp_rpt_class.cdk90 -arch SX5 
		echo " +++++++++++++++++++++++++++++++++"	
	        r.compile -src burp_file_class.cdk90 -arch SX5 
		echo " +++++++++++++++++++++++++++++++++"	
	        r.compile -src burp_file.f90  -arch SX5
		echo " +++++++++++++++++++++++++++++++++"	
		r.build -obj *.o  -o fiche1 -librmn  -arch SX5


tidy :
	rm -f *.o

clean : tidy
	rm -f *.export *.so *.sl *.a *.mod fiche1
