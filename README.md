BURP C Applications Programming Interface

To Compile

1) Clone repository

2) cd in repository

3) Checkout the desired branch

4) Build 
`
. ssmuse-sh -d /fs/ssm/eccc/cmd/cmds/dev/che
. ${CHE_CI_BUILD_ENV}
cmds_che_fix_paths

mkdir -p ${BUILD_DIR}
cd ${BUILD_DIR}
cmake ${BUILD_SRC_DIR} -DCOMPILER_SUITE=intel \
        -DCMAKE_BUILD_TYPE=Release  \
        -DCMAKE_INSTALL_PREFIX="${BUILD_INSTALL_PREFIX}"

make

make test
`

5) Make ssm package

`make ssmpackage`
