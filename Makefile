.SUFFIXES:

.SUFFIXES : .ftn .f .cdk .o

SHELL = /bin/sh

FFLAGS =

CFLAGS =

OPTIMIZ = -O 2
$(info OPTIMIZ is ${OPTIMIZ})

REV = 3.14.0

LIBRMN = rmn 

default: absolu

.ftn.o:
	s.compile -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

.c.o:
	s.compile -abi $(ABI) $(OPTIMIZ) -opt "=$(CFLAGS)" -src $<

.f.o:
	s.compile -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<


OBJECTS= \
	 barf.o 	 bdesire.o 	 bexdes.o 	 brpcopi.o \
	 editbrp.o 	 fermbs.o 	 jhmenm.o 	 julm.o \
	 opodate.o 	 ouvrebs.o 	 restdez.o 	 spool.o \
         jdatxx.o        prefix.o

FICHIERS= \
	 barf.f 	 bdesire.f 	 bexdes.f 	 brpcopi.f \
	 editbrp.f 	 fermbs.f 	 jhmenm.f 	 julm.f \
	 opodate.f 	 ouvrebs.f 	 restdez.f 	 spool.f \
         jdatxx.f        prefix.f

FTNDECKS= \
	 barf.ftn 	 bdesire.ftn 	 bexdes.ftn 	 brpcopi.ftn \
	 editbrp.ftn 	 fermbs.ftn 	 jhmenm.ftn 	 julm.ftn \
	 opodate.ftn 	 ouvrebs.ftn 	 restdez.ftn 	 spool.ftn \
         jdatxx.ftn      prefix.ftn

COMDECKS= \
	 char.cdk 	 desrs.cdk 	 fiches.cdk \
	 maxprms.cdk


jdatxx.o: jdatxx.ftn
barf.o: barf.ftn
barf.o: maxprms.cdk
barf.o: desrs.cdk
barf.o: char.cdk
barf.o: fiches.cdk
bdesire.o: bdesire.ftn
bdesire.o: maxprms.cdk
bdesire.o: desrs.cdk
bdesire.o: fiches.cdk
bdesire.o: char.cdk
bexdes.o: bexdes.ftn
bexdes.o: maxprms.cdk
bexdes.o: desrs.cdk
bexdes.o: fiches.cdk
brpcopi.o: brpcopi.ftn
brpcopi.o: maxprms.cdk
brpcopi.o: fiches.cdk
brpcopi.o: char.cdk
editbrp.o: editbrp.ftn
editbrp.o: maxprms.cdk
editbrp.o: fiches.cdk
editbrp.o: desrs.cdk
editbrp.o: char.cdk
fermbs.o: fermbs.ftn
fermbs.o: maxprms.cdk
fermbs.o: char.cdk
fermbs.o: fiches.cdk
jhmenm.o: jhmenm.ftn
julm.o: julm.ftn
julm.o: maxprms.cdk
julm.o: desrs.cdk
opodate.o: opodate.ftn
opodate.o: maxprms.cdk
opodate.o: desrs.cdk
opodate.o: fiches.cdk
ouvrebs.o: ouvrebs.ftn
ouvrebs.o: maxprms.cdk
ouvrebs.o: char.cdk
ouvrebs.o: fiches.cdk
restdez.o: restdez.ftn
restdez.o: maxprms.cdk
restdez.o: desrs.cdk
spool.o: spool.ftn
spool.o: maxprms.cdk
spool.o: desrs.cdk
spool.o: fiches.cdk
spool.o: char.cdk
prefix.o: prefix.ftn

absolu: $(OBJECTS) 
	s.compile -o editbrp_$(REV)-$(BASE_ARCH) -obj $(OBJECTS) $(OPTIMIZ) -librmn $(LIBRMN)

clean:
#Faire le grand menage. On enleve tous les fichiers sources\ninutiles et les .o 
	-if [ "*.ftn" != "`echo *.ftn`" ] ; \
	then \
	for i in *.ftn ; \
	do \
	fn=`r.basename $$i '.ftn'`; \
	rm -f $$fn.f; \
	done \
	fi
	rm *.o editbrp_$(REV)-$(BASE_ARCH)
