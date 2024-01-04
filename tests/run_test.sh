#!/bin/bash
set -ex

SCRIPT=`readlink -f $0`
SCRIPT_PATH=`dirname $SCRIPT`
#cd $TMPDIR

BINARY=${SCRIPT_PATH}/$1

TESTDIR=${SCRIPT_PATH}/tmp

echo "BINARY=$BINARY"
echo "TESTDIR=$TESTDIR"

mkdir -p ${TESTDIR}
cd $TESTDIR

# load cmoi domain for AFSISIO environment variable (required by rmnlib for qrbsct function)
. ssmuse-sh -d /fs/ssm/eccc/cmo/cmoi/base/20231205

directory=${CMCADE}/banco/postalt/r2
today=`date +%Y%m%d`00_ua

if [ ! -f ${today} ]
then
  cp $directory/$today .
fi

$BINARY ./$today

exit 0
# readburp and readfloat produce too much output for testing so have been removed from list
set -A files read1 setit maxlen obs readcc write2 write2f
for file in ${files[@]}; do
    echo $file
    read "Enter"
    $SCRIPT_PATH/$file ./$today
done

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

