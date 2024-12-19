#!/bin/bash

export IDL_PATH="../../share/scripts:${IDL_PATH}"
export LD_LIBRARY_PATH=../../src/libburpidl/:${LD_LIBRARY_PATH}
export MA_TABLEBURP_PERSONNELLE=./tableburp

echo `pwd`

output=$(idl -e test | grep BWTN6)

if [[ "${output}" != "BWTN6    " ]]; then
    echo "ERROR: Unexpected output from test program"
    exit 1
fi
