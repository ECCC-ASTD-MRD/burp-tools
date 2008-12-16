r.compile -src i_idl_burp2001.c
r.build -obj i_idl_burp2001.o -o i_idl_burp2001.so -shared -conly -librmn rmnshared -optc =-Wl,-rpath,$ARMNLIB/lib/$ARCH
