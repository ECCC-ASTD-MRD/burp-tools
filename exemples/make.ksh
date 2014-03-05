#!/usr/bin/ksh
set -ex

rm -rf *.mod core *.o arjen.db toto titi *.a tit *.export *.so *.sl \
       file_write1f file_write2f file_write3 like_liburp like_liburp2 \
       read_burp write_burp read_config \
       read1 read2 write1 write2 write3 write1f write2f write3f write_update prog_test

. s.ssmuse.dot devtools legacy rmnlib-dev

# use compilers that are compatible with latest rmnlib: rmnlib_013
if [ "${ORDENV_PLAT}" = "aix-7.1-ppc7-64" ]; then
    . s.ssmuse.dot Xlf13
    archive_parameter="-X64"
elif [ "${ORDENV_PLAT}" = "ubuntu-10.04-amd64-64" ]; then
    . s.ssmuse.dot pgi9xx
else
   echo "Unsupported architecture: ${ORDENV_PLAT}"
   exit 1
fi

set -A files read1.f90 read2.f90 like_liburp.f90 like_liburp2.f90 write1.f90 write1f.f90 write2.f90 write2f.f90 write3.f90 write3f.f90 write_update.f90 prog_test.f90
for file in ${files[@]}; do
    echo $file
    binary=`echo $file | cut -f1 -d"."`
    s.compile -o $binary -src $file -includes ../include -libpath ../lib -libappl burp_module -librmn rmn_013 -optf=-g
done

