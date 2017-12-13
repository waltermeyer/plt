#!/bin/bash

# Author: Walter Meyer
# A wrapper script for compiling Gantry programs

if [ $# -ne 1 ] 
then 
	echo "Gantry Compiler Script Usage:" 
	echo "./gantry_comp.sh program.gty" 
	exit 1
fi

program=${1::-4}
./gantry.native < $1 \
                > $program.ll && llc $program.ll \
		> $program.s && gcc -o $program $program.s \
		  gantrylib_string.o gantrylib_http.o gantrylib_obj.o \
		  gantrylib_nap.o -L/usr/lib/x86_64-linux-gnu -lcurl

