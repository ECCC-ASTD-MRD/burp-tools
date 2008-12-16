S%%HELL=/bin/sh

.SUFFIXES   :.cdk90 .f90 .f .c .o
COMPILE.F90 = r.compile -src
BUILD.F90   = r.build -obj
DEBUG        = -debug
DEBUG        =
FFLAGS       = -optf  -O 2
#FFLAGS       = 
sw_package   = burplib-1.1

LIB_OBJECTS =
BURP_OBJECTS= burp_module.o
BURP_STATIC= libburp_module.a
SUPP_OBJECTS=
#EXPORT_DIR=/users/dor/afsd/hmd/f90_burp
EXPORT_DIR=/users/dor/afsd/hmd/f90_burp
EXPORT_DIR=/users/dor/afsd/hmd/f90_burp_1.2




.cdk90.o               :
	$(COMPILE.F90) $< $(DEBUG)

.f90.o                 :
	$(COMPILE.F90)  $< $(FFLAGS) $(DEBUG)



all:clean library
install:export_lib
ftn90_burp_module            : $(LIB_OBJECTS) $(BURP_OBJECTS)

inc_mode :ftn90_burp_module $(SUPP_OBJECTS)
	@echo "OS type detected: " `/bin/uname`
	@case `/bin/uname` in \
	"IRIX64" )  \
	mkdir -p  include/$(ARCH);\
	cp  *.mod include/$(ARCH)/ ;;\
	"AIX" )  \
	mkdir -p  include/$(ARCH);\
	cp  *.mod include/$(ARCH)/ ;;\
	"HP-UX" )  \
	mkdir -p  include/$(ARCH);\
	cp  *.mod include/$(ARCH)/ ;;\
	"Linux" )  \
	mkdir -p  include/$(ARCH);\
	cp  *.mod include/$(ARCH)/ ;;\
	*) echo "This system is not supported" ;; \
	esac

library  : inc_mode
	@echo "OS type detected: " `/bin/uname`
	@case `/bin/uname` in \
	"IRIX64" )  \
	mkdir -p  lib/$(ARCH);\
        ar rc $(BURP_STATIC) $(BURP_OBJECTS);\
	cp  $(BURP_STATIC)  lib/$(ARCH)/ ;;\
	"AIX" )  \
	mkdir -p  lib/$(ARCH);\
        ar rc $(BURP_STATIC) $(BURP_OBJECTS);\
	cp  $(BURP_STATIC)  lib/$(ARCH)/ ;;\
	"HP-UX" )  \
	mkdir -p  lib/$(ARCH);\
        ar rc $(BURP_STATIC) $(BURP_OBJECTS);\
	cp  $(BURP_STATIC)  lib/$(ARCH)/ ;;\
	"Linux" )  \
	mkdir -p  lib/$(ARCH);\
        ar rc $(BURP_STATIC) $(BURP_OBJECTS);\
	cp  $(BURP_STATIC)  lib/$(ARCH)/ ;;\
	*) echo "This system is not supported" ;; \
	esac

tidy     :
	rm -f *.o 

clean    : tidy
	rm -f *.export *.so *.sl *.a *.mod \
	read1 read2 write1 write2 read_burp \
	write_burp

clean_lib:
	rm -rf include/Linux_pgi611/*
	rm -rf lib/Linux_pgi611/*
	rm -rf include/Linux/*
	rm -rf lib/Linux/*
	rm -rf include/IRIX64/*
	rm -rf lib/IRIX64/*
	rm -rf include/AIX/*
	rm -rf lib/AIX/*

export_lib:
	mkdir -p  $(EXPORT_DIR)/include/$(ARCH)/
	mkdir -p  $(EXPORT_DIR)/lib/$(ARCH)/
#        mkdir -p  $(EXPORT_DIR)/include/IRIX64
#        mkdir -p  $(EXPORT_DIR)/include/AIX
#        mkdir -p  $(EXPORT_DIR)/lib/Linux
#        mkdir -p  $(EXPORT_DIR)/lib/IRIX64
#        mkdir -p  $(EXPORT_DIR)/lib/AIX
#        mkdir -p  $(EXPORT_DIR)/lib/HP90

	mkdir -p  $(EXPORT_DIR)/exemples
	cp -rf exemples/* $(EXPORT_DIR)/exemples/
	cp -rf include/$(ARCH)/* $(EXPORT_DIR)/include/$(ARCH)/
	cp -rf lib/$(ARCH)/* $(EXPORT_DIR)/lib/$(ARCH)/
#        cp -r include/IRIX64/*  $(EXPORT_DIR)/include/IRIX64/
#        cp -r lib/IRIX64/* $(EXPORT_DIR)/lib/IRIX64/
#        cp -r include/AIX/*  $(EXPORT_DIR)/include/AIX/
#        cp -r lib/AIX/* $(EXPORT_DIR)/lib/AIX/
#        cp -r include/HP90/*  $(EXPORT_DIR)/include/HP90/
#        cp -r lib/HP90/* $(EXPORT_DIR)/lib/HP90/
sw_lib:
	mkdir -p  $(HOME)/$(ARCH)/$(sw_package)/include
	mkdir -p  $(HOME)/$(ARCH)/$(sw_package)/lib
	mkdir -p  $(HOME)/$(ARCH)/$(sw_package)/data

	cp -r include/$(ARCH)/* $(HOME)/$(ARCH)/$(sw_package)/include/
	cp -r lib/$(ARCH)/* $(HOME)/$(ARCH)/$(sw_package)/lib/
	cp ./data/DESCRIPTION $(HOME)/$(ARCH)/$(sw_package)/data/
        
