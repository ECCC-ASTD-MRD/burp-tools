
.SUFFIXES : .ftn .f .cdk

SHELL = /bin/sh

FFLAGS =

CFLAGS =

OPTIMIZ = -O 2

LIC = 2.1

default: absolu

.ftn.o:
	s.compile -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

.c.o:
	s.compile -abi $(ABI) $(OPTIMIZ) -opt "=$(CFLAGS)" -src $<

.f.o:
	s.compile -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<


OBJECTS= brpvoir.o

FICHIERS= brpvoir.f

absolu: $(OBJECTS) 
	s.compile -o brpvoir_$(LIC)-$(BASE_ARCH) -obj $(OBJECTS) -arch $(EC_ARCH) -abi $(ABI) -librmn rmn_013

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
	rm *.o brpvoir_$(LIC)-$(BASE_ARCH)
