set -x
r.compile -src i_idl_burp2001.c
ld -o i_idl_63_burp2001.so   -shared i_idl_burp2001.o  -L $ARMNLIB/lib/IRIX64 -l rmn_shared_008 -l ftn -l m -rpath $ARMNLIB/lib/IRIX64



