#! /usr/bin/env bash
set -eux

target=${machine:-wcoss_dell_p3}

cwd=`pwd`

progname=gfsnemsio2grb

source ./${progname}.$target             > /dev/null 2>&1
echo $wgrib2api

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

export INCSM="-I ${NEMSIO_INC} -I ${wgrib2api}"

export LIBSM="${NEMSIO_LIB} ${BACIO_LIB4} ${W3NCO_LIB4} -L${wgrib2api} -lwgrib2"


make -f Makefile clobber
make -f Makefile
make -f Makefile install
make -f Makefile clobber

exit

