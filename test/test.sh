#!/bin/bash

SCRIPT=$(readlink -f ${BASH_SOURCE-${.sh.file}})
SCRIPT_PATH=$(dirname ${SCRIPT})

# load environment
. d.compile.ordenv.cfg
. d.compile.utils.${EC_ARCH}.cfg


export IDL_PATH="../share/scripts:${IDL_PATH}"
export LD_LIBRARY_PATH=../lib/:../src/:${LD_LIBRARY_PATH}
export MA_TABLEBURP_PERSONNELLE=./tableburp

output=$(r.idl -e test | grep BWTN6)

#LD_LIBRARY_PATH=../src/:${LD_LIBRARY_PATH}  r.idl -e test
#LD_LIBRARY_PATH=../src/:${LD_LIBRARY_PATH}  ldd ../src/libburp_idl.so

if [[ "${output}" != "BWTN6    " ]]; then
    echo "ERROR: Unexpected output from test program"
    exit 1
fi
