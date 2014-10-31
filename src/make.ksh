#!/usr/bin/ksh
set -ex

# make sure we are compiling locally
SCRIPT=`readlink -f $0`
SCRIPT_PATH=`dirname $SCRIPT`
cd $SCRIPT_PATH

rm -rf *.mod core *.o arjen.db toto titi *.a tit *.export *.so *.sl read1 read2 write1 write2 read_burp write_burp

# load appropriate compilers for each architecture
if [[ -z ${COMP_ARCH} ]]; then
    if [[ "${ORDENV_PLAT}" = "aix-7.1-ppc7-64" ]]; then
        . ssmuse-sh -d hpcs/201402/01/base -d hpcs/ext/xlf_13.1.0.10
    elif [[ "${ORDENV_PLAT}" = "ubuntu-10.04-amd64-64" || "${ORDENV_PLAT}" = "ubuntu-12.04-amd64-64" ]]; then
        . ssmuse-sh -d hpcs/201402/01/base -d hpcs/201402/01/intel13sp1u2
    else
       echo "Unsupported architecture: ${ORDENV_PLAT}"
       exit 1
    fi
fi

if [ "${ORDENV_PLAT}" = "aix-7.1-ppc7-64" ]; then
    archive_parameter="-X64"
fi

s.compile -src burp_module.f90 -debug -O 2

ls *.o

ar $archive_parameter scru libburp_module.a burp_module.o
ranlib libburp_module.a

cp -rf libburp_module.a ../lib
cp -rf *.mod ../include

