#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

progname=gettrk

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

#FFLAGSM will be used different for f and f90
export FFLAGSM=
export RECURS=
export LDFLAGSM=${LDFLAGSM:-""}
export OMPFLAGM=${OMPFLAGM:-""}

export INCSM="-I ${SIGIO_INC4} -I ${SFCIO_INC4} -I ${NEMSIOGFS_INC} -I ${NEMSIO_INC}"


export LIBSM="${NEMSIOGFS_LIB} ${NEMSIO_LIB} ${BACIO_LIB4} ${SIGIO_LIB4} ${IP_LIB4} ${SP_LIB4} ${SFCIO_LIB4} ${BUFR_LIB4} ${W3EMC_LIB4} ${W3NCO_LIB4}"

make -f Makefile clobber
make -f Makefile
make -f Makefile install
make -f Makefile clobber

exit
