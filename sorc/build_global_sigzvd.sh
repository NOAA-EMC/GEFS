#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

progname=global_sigzvd

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

export FFLAGSM="-mkl -O2  -p -xHOST -r8  -convert big_endian -i4 -list -traceback -assume byterecl"
export RECURS=
export LDFLAGSM=${LDFLAGSM:-""}
export OMPFLAGM=${OMPFLAGM:-""}

export INCSM="-I ${SIGIO_INC4}"

export LIBSM="${NEMSIOGFS_LIB} ${NEMSIO_LIB} ${SP_LIBd} ${SIGIO_LIB4} ${W3NCO_LIBd} ${BACIO_LIB4}"
#export LIBSM="${SIGIO_LIB4}"

make -f Makefile clobber
make -f Makefile
make -f Makefile install
make -f Makefile clobber

exit
