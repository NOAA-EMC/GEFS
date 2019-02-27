#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

progname=gefs_vortex_combine

source ../modulefiles/gefs/${progname}.$target             > /dev/null 2>&1

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

#
#
cd ${progname}.fd

export FCMP=${FCMP:-ifort}
export FCMP95=$FCMP

export FFLAGSM="-p  -convert big_endian -r8 -O0 -i4 -traceback -assume byterecl -list"
export RECURS=
export LDFLAGSM=${LDFLAGSM:-""}
export OMPFLAGM=${OMPFLAGM:-""}

export INCSM="-I ${SIGIO_INC4} -I ${NEMSIO_INC} -I ${NEMSIOGFS_INC}"

export LIBSM="${NEMSIOGFS_LIB} ${NEMSIO_LIB} ${SP_LIBd} ${SIGIO_LIB4} ${W3NCO_LIBd} ${BACIO_LIB4}"


make -f Makefile clobber
make -f Makefile
make -f Makefile install
make -f Makefile clobber

exit
