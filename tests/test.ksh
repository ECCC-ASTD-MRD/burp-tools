#!/usr/bin/ksh
set -ex

SCRIPT=`readlink -f $0`
SCRIPT_PATH=`dirname $SCRIPT`
cd $TMPDIR

directory=/data/ade/banco/postalt/r1
today=`date +%Y%m%d`00_

rm -rf *_
scp joule:$directory/$today .

# load cmoi domain for AFSISIO environment variable (required by rmnlib for qrbsct function)
. ssmuse-sh -d /ssm/net/cmoi/base/20130927

# like_liburp2 produces too much output for testing purposes
# prog_test is meant to work on a very specific file so has temporarily been removed from the list of programs
# write1f removed - no disk space in $TMPDIR on hadar/spica to run this test
set -A files read1 read2 like_liburp write1 write2 write2f write3 write3f write_update
for file in ${files[@]}; do
    echo $file
    read "Enter"
    $SCRIPT_PATH/$file ./$today
done

# cleanup temporary files that were created by the above programs
rm -rf *_ file_write1 file_write1f

