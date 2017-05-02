#!/bin/bash

SCRIPT=$(readlink -f ${BASH_SOURCE-${.sh.file}})
SCRIPT_PATH=$(dirname ${SCRIPT})

# load environment
#. d.compile.ordenv.cfg
#. d.compile.intel.cfg
. d.compile
export IDL_PATH="../share/scripts:${IDL_PATH}"
# a little bit of help to let old ordenv find librmnshared
export LD_LIBRARY_PATH=/ssm/net/rpn/libs/${RMNLIB_DOMAIN}/${ORDENV_PLAT}/lib/Linux_x86-64/${COMP_ARCH}/:${SCRIPT_PATH}/../src/:${LD_LIBRARY_PATH};
#echo ${LD_LIBRARY_PATH};

output=$(r.idl -e test | grep BWTN6)

if [[ "${output}" != "BWTN6    " ]]; then
    echo "ERROR: Unexpected output from test program"
    exit 1
fi
