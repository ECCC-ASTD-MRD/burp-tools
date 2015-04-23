#!/usr/bin/ksh
set -e

# make sure we are compiling locally
SCRIPT=`readlink -f $0`
SCRIPT_PATH=`dirname $SCRIPT`
cd $SCRIPT_PATH

rm -rf core *.o *.a *.so 

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
. ssmuse-sh -d rpn/libs/15.2
# for $IDL_DIR idl_export.h
. ssmuse-sh -d hpcs/ext/idl82

if [[ "${ORDENV_PLAT}" = "ubuntu-10.04-amd64-64" || "${ORDENV_PLAT}" = "ubuntu-12.04-amd64-64" ]]; then
    if [[ "${COMP_ARCH}" == "intel13sp1u2" ]]; then
        # need to be able to find librmnshared_015.1.so and libimf.so etc. at run-time
        compiler_parameters="-optc =-fp-model =precise =-Wl,-rpath=/ssm/net/rpn/libs/15.2/ubuntu-10.04-amd64-64/lib/Linux_x86-64/intel13sp1u2,-rpath=/ssm/net/hpcs/201402/01/intel13sp1u2/multi/lib/intel64"
    fi
fi

set -ex

s.compile -o libburp_idl.so -src libburp_idl.c -shared -conly -includes $IDL_DIR/external/include $compiler_parameters -librmn rmnshared_015.2

cp -rf libburp_idl.so ../lib

