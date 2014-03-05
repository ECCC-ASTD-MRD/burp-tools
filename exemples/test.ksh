#!/usr/bin/ksh
set -ex

SCRIPT=`readlink -f $0`
SCRIPT_PATH=`dirname $SCRIPT`
cd $TMPDIR

directory=/data/ade/banco/postalt/r1
today=`date +%Y%m%d`00_

rm -rf *_
scp joule:$directory/$today .

# like_liburp2 produces too much output for testing purposes
# prog_test is meant to work on a very specific file so has temporarily been removed from the list of programs
set -A files read1 read2 like_liburp write1 write1f write2 write2f write3 write3f write_update
for file in ${files[@]}; do
    echo $file
    read "Enter"
    $SCRIPT_PATH/$file ./$today
done

# cleanup temporary files that were created by the above programs
rm -rf *_ file_write1 file_write1f

