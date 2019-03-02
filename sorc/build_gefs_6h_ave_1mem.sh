#! /usr/bin/env bash
set -eux

target=${machine:-wcoss_dell_p3}

cwd=`pwd`

progname=gefs_6h_ave_1mem

source ./${progname}.$target             > /dev/null 2>&1

# Check final exec folder exists
if [ ! -d "../exec" ]; then
    mkdir ../exec
fi

#
#
cd ${progname}.fd

export FCMP=${FCMP:-ifort}
export FCMP95=$FCMP

export FFLAGSM="-O" #3 -g -convert big_endian"
export RECURS=
export LDFLAGSM=${LDFLAGSM:-""}
export OMPFLAGM=${OMPFLAGM:-""}

export INCSM="-I ${G2_INC4}"

export LIBSM="${G2_LIB4} ${BACIO_LIB4} ${IP_LIB4} ${W3NCO_LIB4} ${SP_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB}"


make -f Makefile clobber
make -f Makefile
make -f Makefile install
make -f Makefile clobber

exit

