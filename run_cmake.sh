#!/bin/bash

. ssmuse-sh -d /fs/ssm/eccc/cmd/cmds/dev/che
. ${CHE_CI_BUILD_ENV}
# the above line will define
#    BUILD_SRC_DIR        : where source is
#    BUILD_DIR            : $SRC_DIR/build
#    BUILD_INSTALL_PREFIX : $BUILD_DIR/intall_test.dir to avoid default installing in /usr
cmds_che_fix_paths

echo "Using cmake :  $(which cmake)"

mkdir -p ${BUILD_DIR} 
cd ${BUILD_DIR}
cmake ${BUILD_SRC_DIR} -DCOMPILER_SUITE=intel \
        -DCMAKE_BUILD_TYPE=Release  \
        -DCMAKE_INSTALL_PREFIX="${BUILD_INSTALL_PREFIX}" 

status=$?
if [ $status -ne 0 ]
then
  echo "Error running cmake"
  exit 1
fi

VERBOSE=1 make
VERBOSE=1 make test
status=$?
if [ $status -ne 0 ]
then
   echo "Error running tests"
   exit 1
fi

make ssmpackage

exit 0
