#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

progname=overenstr.grib

USE_PREINST_LIBS=${USE_PREINST_LIBS:-"true"}
if [ $USE_PREINST_LIBS = true ]; then
    export MOD_PATH=/scratch3/NCEPDEV/nwprod/lib/modulefiles
    source ../modulefiles/gefs/${progname}.$target             > /dev/null 2>&1
else
    export MOD_PATH=${cwd}/lib/modulefiles
    source ../modulefiles/gefs/${progname}.$target             > /dev/null 2>&1
fi

cd ../util/sorc/


# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

#
#
cd ${progname}.fd

export FCMP=${FCMP:-ifort}
export FCMP95=$FCMP

export FFLAGSM=""
export RECURS=
export LDFLAGSM=${LDFLAGSM:-""}
export OMPFLAGM=${OMPFLAGM:-""}

export INCSM=""

export LIBSM="${W3NCO_LIBd} ${BACIO_LIB4}"

make -f Makefile2 clobber
make -f Makefile2
make -f Makefile2 install
make -f Makefile2 clobber

exit
