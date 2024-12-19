#!/bin/bash

SCRIPT=$(readlink -f ${BASH_SOURCE-${.sh.file}})
SCRIPT_PATH=$(dirname ${SCRIPT})

# load environment
. d.compile.ordenv.cfg
. d.compile.utils.${EC_ARCH}.cfg


export IDL_PATH="../../share/scripts:${IDL_PATH}"
export LD_LIBRARY_PATH=../lib/:../src/:${LD_LIBRARY_PATH}
export MA_TABLEBURP_PERSONNELLE=./tableburp

output=$(r.idl -e test | grep BWTN6)

if [[ "${output}" != "BWTN6    " ]]; then
    echo "ERROR: Unexpected output from test program"
    exit 1
fi
