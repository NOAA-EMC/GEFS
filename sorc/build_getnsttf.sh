#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

progname=getnsttf

USE_PREINST_LIBS=${USE_PREINST_LIBS:-"true"}
if [ $USE_PREINST_LIBS = true ]; then
    export MOD_PATH=/scratch3/NCEPDEV/nwprod/lib/modulefiles
    source ../modulefiles/gefs/${progname}.$target             > /dev/null 2>&1
else
    export MOD_PATH=${cwd}/lib/modulefiles
    source ../modulefiles/gefs/${progname}.$target             > /dev/null 2>&1
fi


cd ../util/sorc

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

#
#
cd ${progname}.fd

export FCMP=${FCMP:-ifort}
export FCMP95=$FCMP

export FFLAGSM="-O3 -fp-model source -convert big_endian -assume byterecl -implicitnone"
export RECURS=
export LDFLAGSM=${LDFLAGSM:-""}
export OMPFLAGM=${OMPFLAGM:-""}

export INCSM="-I ${SFCIO_INC4} -I ${NEMSIO_INC}"


export LIBSM="${NEMSIO_LIB} ${BACIO_LIB4} ${SFCIO_LIB4} ${W3NCO_LIB4}"

make -f Makefile2 clobber
make -f Makefile2
make -f Makefile2 install
make -f Makefile2 clobber

exit
