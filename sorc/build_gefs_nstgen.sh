#! /usr/bin/env bash
set -eux

source ./machine-setup.sh
cwd=`pwd`

progname=gefs_nstgen

if [ -f ../rocoto/dev/versions/build_$target.ver ]; then
    source ../rocoto/dev/versions/build_$target.ver
else
    if [ -f ../versions/build.ver ]; then
        source ../versions/build.ver
    fi
fi

if [[ $target == "wcoss2" ]] || [[ $target == "wcoss_dell_p3" ]]; then
    module use ${cwd}/../modulefiles/gefs
    module load ${progname}.${target}.lua
else
    source ../modulefiles/gefs/${progname}.$target
fi

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

#
cd ${progname}.fd

export FCMP=${FCMP:-ifort}
export FCMP95=$FCMP

export FFLAGSM="-O3 -fp-model source -convert big_endian -assume byterecl -implicitnone"
export RECURS=
export LDFLAGSM=${LDFLAGSM:-""}
export OMPFLAGM=${OMPFLAGM:-""}

export INCSM="-I ${NETCDF_INCLUDES}"

export LIBSM="${NETCDF_LDFLAGS} ${BACIO_LIB4} ${W3NCO_LIB4}"


make -f Makefile clobber
make -f Makefile
make -f Makefile install
make -f Makefile clobber

exit
