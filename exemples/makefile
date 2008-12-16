SHELL=/bin/sh

.SUFFIXES   :.cdk90 .f90 .f .c .o
COMPILE.F90 = r.compile -src
BUILD.F90   = r.build -obj
DEBUG        = -debug
DEBUG        =
FFLAGS       = -optf=-g

DIR_LIB      =..
#DIR_LIB      =/users/dor/afsd/hmd/f90_burp
DSSM=/home/cmss/afsm/adm/ssm_access_points/cmda
PKGN=burplib
VERN=1.2


ifeq ($(ARCH),AIX)
   OS=aix51-ppc-64
   CCP=xlC
   LIBRMNSHARED=rmnshared
   LD_FLAGS=
endif

ifeq ($(ARCH),Linux_pgi611)
   OS=linux26-i386
   CCP=g++
   LIBRMNSHARED=rmn_shared
   DEFINES= -defines =-DLinux
   LD_FLAGS= -Wl,--allow-shlib-undefined  -Wl,-rpath,$(ARMNLIB)/lib/$(ARCH)
endif
ifeq ($(ARCH),IRIX64)
   OS=irix65-mips-n32
   CCP=CC
   LIBRMNSHARED=rmnshared
   LD_FLAGS= -Wl,-rpath,$(ARMNLIB)/lib/$(ARCH)
endif

UDS=_
DIR_LIB=$(DSSM)/$(PKGN)$(UDS)$(VERN)$(UDS)$(OS)


.cdk90.o               :
	$(COMPILE.F90) $< $(DEBUG) $(FFLAGS)

.f90.o                 :
	$(COMPILE.F90) $< $(DEBUG) $(FFLAGS)



all:read1 read2 like_liburp write1 write2 write3 write_update prog_test
read1:read1.f90
	$(COMPILE.F90) $@.f90 -includes $(DIR_LIB)/include  -libpath $(DIR_LIB)/lib -libappl burp_module -librmn $(DEBUG) $(FFLAGS) -o  $@ 

read2:read2.f90
	$(COMPILE.F90) $@.f90 -includes $(DIR_LIB)/include  -libpath $(DIR_LIB)/lib -libappl burp_module -librmn $(DEBUG) $(FFLAGS) -o  $@ 

like_liburp: like_liburp.f90
	$(COMPILE.F90) $@.f90 -includes $(DIR_LIB)/include  -libpath $(DIR_LIB)/lib -libappl burp_module -librmn $(DEBUG) $(FFLAGS) -o  $@ 

like_liburp2: like_liburp2.f90
	$(COMPILE.F90) $@.f90 -includes $(DIR_LIB)/include  -libpath $(DIR_LIB)/lib -libappl burp_module -librmn $(DEBUG) $(FFLAGS) -o  $@ 
write1: write1.f90
	$(COMPILE.F90) $@.f90 -includes $(DIR_LIB)/include  -libpath $(DIR_LIB)/lib -libappl burp_module -librmn $(DEBUG)  -o  $@ 

write1f: write1f.f90
	$(COMPILE.F90) $@.f90 -includes $(DIR_LIB)/include  -libpath $(DIR_LIB)/lib -libappl burp_module -librmn $(DEBUG) $(FFLAGS)  -o  $@ 


write2:write2.f90
	$(COMPILE.F90) $@.f90 -includes $(DIR_LIB)/include  -libpath $(DIR_LIB)/lib -libappl burp_module -librmn $(DEBUG) $(FFLAGS) -o  $@ 

write2f:write2f.f90
	$(COMPILE.F90) $@.f90 -includes $(DIR_LIB)/include  -libpath $(DIR_LIB)/lib -libappl burp_module -librmn $(DEBUG) $(FFLAGS) -o  $@ 

write3:write3.f90
	$(COMPILE.F90) $@.f90 -includes $(DIR_LIB)/include  -libpath $(DIR_LIB)/lib -libappl burp_module -librmn $(DEBUG) $(FFLAGS) -o  $@ 

write3f:write3f.f90
	$(COMPILE.F90) $@.f90 -includes $(DIR_LIB)/include  -libpath $(DIR_LIB)/lib -libappl burp_module -librmn $(DEBUG) $(FFLAGS) -o  $@ 

write_update:write_update.f90
	$(COMPILE.F90) $@.f90 -includes $(DIR_LIB)/include  -libpath $(DIR_LIB)/lib -libappl burp_module -librmn $(DEBUG) $(FFLAGS) -o  $@ 

prog_test:prog_test.f90
	$(COMPILE.F90) $@.f90 -includes $(DIR_LIB)/include  -libpath $(DIR_LIB)/lib -libappl burp_module -librmn $(DEBUG) $(FFLAGS) -o  $@ 

tidy     :
	rm -f *.o

clean    : tidy
	rm -f *.export *.so *.sl *.a *.mod \
        file_write1f file_write2f file_write3 like_liburp like_liburp2\
        read_burp write_burp read_config\
        read1 read2 write1 write2 write3 write1f write2f write3f write_update prog_test

