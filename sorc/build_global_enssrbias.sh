#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

progname=global_enssrbias

USE_PREINST_LIBS=${USE_PREINST_LIBS:-"true"}
if [ $USE_PREINST_LIBS = true ]; then
    export MOD_PATH=/scratch3/NCEPDEV/nwprod/lib/modulefiles
    source ../modulefiles/gefs/${progname}.$target             > /dev/null 2>&1
else
    export MOD_PATH=${cwd}/lib/modulefiles
    source ../modulefiles/gefs/${progname}.$target             > /dev/null 2>&1
fi

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

#
#
cd ${progname}.fd

export FCMP=${FCMP:-ifort}
export FCMP95=$FCMP

export FFLAGSM="-list -traceback -convert big_endian -assume byterecl"
export RECURS=
export LDFLAGSM=${LDFLAGSM:-""}
export OMPFLAGM=${OMPFLAGM:-""}

export INCSM=

export LIBSM=

make -f Makefile clobber
make -f Makefile
make -f Makefile install
make -f Makefile clobber

exit
