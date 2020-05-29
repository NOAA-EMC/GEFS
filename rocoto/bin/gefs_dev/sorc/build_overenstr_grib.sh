#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`
export cwdm=${cwd}/

progname=overenstr.grib

if [ -f ../modulefiles/gefs/gefs_$target.ver ]; then
    source ../modulefiles/gefs/gefs_$target.ver
fi
source ../modulefiles/gefs/${progname}.$target             > /dev/null 2>&1

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

make -f Makefile clobber
make -f Makefile
make -f Makefile install
make -f Makefile clobber

exit
