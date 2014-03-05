#!/usr/bin/ksh
set -ex

# make sure we are compiling locally
SCRIPT=`readlink -f $0`
SCRIPT_PATH=`dirname $SCRIPT`
cd $SCRIPT_PATH

rm -rf *.mod core *.o arjen.db toto titi *.a tit *.export *.so *.sl read1 read2 write1 write2 read_burp write_burp

. s.ssmuse.dot devtools

# use compilers that are compatible with latest rmnlib: rmnlib_013
if  [ "${ORDENV_PLAT}" = "aix-7.1-ppc7-64" ]
then
   . s.ssmuse.dot Xlf13
   archive_parameter="-X64"
elif [ "${ORDENV_PLAT}" = "ubuntu-10.04-amd64-64" ]
then
   . s.ssmuse.dot pgi9xx
else
   echo "Unsupported architecture: ${ORDENV_PLAT}"
   exit 1
fi

# -debug
s.compile -src burp_module.f90 -optf -O 2

ls *.o

ar $archive_parameter scru libburp_module.a burp_module.o
ranlib libburp_module.a

cp -rf libburp_module.a ../lib
cp -rf *.mod ../include

