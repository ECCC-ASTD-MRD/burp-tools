#!/bin/sh
r.compile -includes /sw/sgi4D-pub-6/idl-5.4/lib/idl_5.4/external -src i_idl_burp2001.c
 
ld -shared -n32  -o i_idl_burp2001.so i_idl_burp2001.o -L $ARMNLIB/lib/IRIX64/SO -l rmn -rpath $ARMNLIB/lib/IRIX64/SO
#ld -shared -n32 -mips3 -o i_idl_burp2001.so i_idl_burp2001.o -L $ARMNLIB/lib/IRIX64/SO -l rmn -rpath $ARMNLIB/lib/IRIX64/SO
