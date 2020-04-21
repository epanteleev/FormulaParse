#!/bin/bash 
NASM_PATH=/home/user/Documents/FormulaParse/bin/bin
$NASM_PATH/nasm -f elf64 $1 -o "$1.o"
if [ -f $1.o ]; then
	gcc -m64 "$1.o" -o "$1-run"	
fi;
