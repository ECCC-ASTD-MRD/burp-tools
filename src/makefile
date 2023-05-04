all:
	# deplacer rmn dans l'argument "libsys" afin d'attraper la version .so de rmnlib.
	d.compile -o libburp_idl.so -src libburp_idl.c -conly -libsys rmn -libappl irc -optc -allow-undefined 
	#d.compile -o libburp_idl.so -src libburp_idl.c -conly -librmn rmn -libappl irc -optc -allow-undefined 



# commande generee par d/s.compile, j'ai du deplacer le "-lrmn" apres l'argument "-Bdynamic"
	#icc -shared -L/home/ant000/userlibs/rhel-8-icelake-64/inteloneapi-2022.1.2 -L/fs/ssm/eccc/cmd/cmds/ext/20230215/rhel-8-amd64-64/lib -L/fs/ssm/eccc/mrd/rpn/libs/20230414/rhel-8-icelake-64/inteloneapi-2022.1.2/lib -L/fs/ssm/eccc/mrd/rpn/libs/20230414/rhel-8-amd64-64/inteloneapi-2022.1.2/lib -L/fs/ssm/eccc/cmd/cmda/libs/20230414/inteloneapi-2022.1.2/rhel-8-icelake-64/lib -L/fs/ssm/eccc/cmd/cmda/utils/20230414/rhel-8-icelake-64/lib -L/lib/rhel-8-icelake-64/inteloneapi-2022.1.2 -Wl,--allow-shlib-undefined -Wtrigraphs -traceback -fpic -I. -xCORE-AVX512 -fp-model precise -O1 -g -traceback -fp-model precise -Wall -Wl,--export-dynamic -DLittle_Endian -DWITH_intel -D__INTEL_COMPILER=2021 -DAMD64 -DLINUX_X86_64 -DWITHOUT_OpenMP -o libburp_idl.so libburp_idl.o -lirc -Bstatic -lApp -Bdynamic -lrmn
