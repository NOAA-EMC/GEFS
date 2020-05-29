#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`
export cwdm=${cwd}/

progname=global_enspvrfy

if [ -f ../modulefiles/gefs/gefs_$target.ver ]; then
    source ../modulefiles/gefs/gefs_$target.ver
fi
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

export FFLAGSM="-O3 -g -r8 -convert big_endian -auto"
export RECURS=
export LDFLAGSM=${LDFLAGSM:-""}
export OMPFLAGM=${OMPFLAGM:-""}

export INCSM="-I ${G2_INCd}"

export LIBSM="${G2_LIBd} ${W3NCO_LIBd} ${BACIO_LIB4} ${IP_LIBd} ${SP_LIBd} ${PNG_LIB} ${JASPER_LIB} ${Z_LIB} ${W3NCO_LIBd}"

make -f Makefile clobber
make -f Makefile
make -f Makefile install
make -f Makefile clobber

exit
