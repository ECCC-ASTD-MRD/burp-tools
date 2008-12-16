r.compile -src i_idl_burp2001.c
r.build -obj i_idl_burp2001.o -o i_idl_63_burp2001.so -shared -conly -librmn rmn_shared_008  -optc =-Wl,-rpath,$ARMNLIB/lib/$ARCH
