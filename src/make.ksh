#!/usr/bin/ksh
set -ex

# make sure we are compiling locally
SCRIPT=`readlink -f $0`
SCRIPT_PATH=`dirname $SCRIPT`
cd $SCRIPT_PATH

rm -rf core *.o *.a *.so 

# load appropriate compilers for each architecture
if [[ -z ${COMP_ARCH} ]]; then
    if [[ "${ORDENV_PLAT}" = "aix-7.1-ppc7-64" ]]; then
        . ssmuse-sh -d hpcs/201402/01/base -d hpcs/ext/xlf_13.1.0.10
        # for rmnlib.h
        . ssmuse-sh -d rpn/libs/15.1
        # for $IDL_DIR idl_export.h
        . ssmuse-sh -d hpcs/ext/idl82

        #s.compile -o libburp_ido.so -src libburp_idl.c -shared -includes /ssm/net/hpcs/ext/idl82/idl_8.2.3_ubuntu-10.04-amd64-64/idl82/external/include -optf =-static-intel -librmn rmn_015.1
        s.compile -o libburp_idl.so -src libburp_idl.c -shared -conly -includes $IDL_DIR/external/include $compiler_parameters -librmn rmnshared_015.1
    elif [[ "${ORDENV_PLAT}" = "ubuntu-10.04-amd64-64" || "${ORDENV_PLAT}" = "ubuntu-12.04-amd64-64" ]]; then
        #. ssmuse-sh -d hpcs/201402/02-test/base -d hpcs/201402/02-test/intel13sp1u2
        . ssmuse-sh -d hpcs/201402/01/base -d hpcs/201402/01/intel13sp1u2
        # for rmnlib.h
        . ssmuse-sh -d rpn/libs/15.1
        # for $IDL_DIR idl_export.h
        . ssmuse-sh -d hpcs/ext/idl82

        # need to be able to find librmnshared_015.1.so and libimf.so etc. at run-time
        compiler_parameters="-optc =-Wl,-rpath=/ssm/net/rpn/libs/15.1/ubuntu-10.04-amd64-64/lib/Linux_x86-64/intel13sp1u2,-rpath=/ssm/net/hpcs/201402/01/intel13sp1u2/multi/lib/intel64"

        #s.compile -o libburp_ido.so -src libburp_idl.c -shared -includes /ssm/net/hpcs/ext/idl82/idl_8.2.3_ubuntu-10.04-amd64-64/idl82/external/include -optf =-static-intel -librmn rmn_015.1
        s.compile -o libburp_idl.so -src libburp_idl.c -shared -conly -includes $IDL_DIR/external/include $compiler_parameters -librmn rmnshared_015.1
    elif [[ "${ORDENV_PLAT}" = "debian-4.0-i686-32" ]]; then
        . s.ssmuse.dot devtools rmnlib-dev pgi9xx external-sw

        includes=`idl -inc`
	compiler_parameters="-optc =-Wl,-rpath=/home/ordenv/ssm-domains1/ssm-rmnlib-dev/rmnlib-source_beta_multi/lib/Linux_pgi9xx"

        s.compile -o libburp_idl.so -src libburp_idl.c -shared -conly -includes $includes $compiler_parameters -librmn rmnshared_012
    else
       echo "Unsupported architecture: ${ORDENV_PLAT}"
       exit 1
    fi
fi

cp -rf libburp_idl.so ../lib

