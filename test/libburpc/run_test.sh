#!/bin/bash
#set -ex

SCRIPT=`readlink -f $0`
SCRIPT_PATH=`dirname $SCRIPT`
#cd $TMPDIR

if [ $# -eq 0 ]
then
   echo "error: binary not given"
   exit 1
fi

if [ $# -ge 1 ]
then
   BINARY=${SCRIPT_PATH}/$1
fi

if [ $# -ge 2 ]
then
   PRE_ARG=$2
fi

if [ $# -ge 2 ]
then
   POST_ARG=$3
fi

TESTDIR=${SCRIPT_PATH}/tmp

mkdir -p ${TESTDIR}
cd $TESTDIR

# load cmoi domain for AFSISIO environment variable (required by rmnlib for qrbsct function)
. ssmuse-sh -d /fs/ssm/eccc/cmo/cmoi/base/20231205

RMN_RELEASE=@RMN_RELEASE@
. r.load.dot mrd/rpn/libs/$RMN_RELEASE
. r.load.dot mrd/rpn/utils/$RMN_RELEASE
. r.load.dot mrd/rpn/diag/$RMN_RELEASE

. r.load.dot rpn/code-tools/ENV/cdt-1.6.6/SCIENCE/inteloneapi-2022.1.2


directory=${CMCADE}/banco/postalt/r2
today=`date +%Y%m%d`00_ua

if [ ! -f ${today} ]
then
  cp $directory/$today .
fi

$BINARY ${PRE_ARG} ./$today ${POST_ARG}

exit 0

set -A files elements val
for file in ${files[@]}; do
    echo $file
    read "Enter"
    $SCRIPT_PATH/$file -f ./$today
done

echo "write1"
read "Enter"
$SCRIPT_PATH/write1 ./$today ./junk_output

# cleanup temporary files that were created by the above programs
rm -rf *_ file_write1 file_write1f ./junk_output

