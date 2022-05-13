#! /usr/bin/env bash
set -eux

source ./machine-setup.sh
cwd=`pwd`

progname=gefs_nemsio2nc

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
cd ${progname}.cd

export FCMP=${FCMP:-ifort}
export FCMP95=$FCMP

export CCMP=${CCMP:-icc} #icpc} #icc}

export CXX_FLAGSM="-std=gnu++11"

export FFLAGSM= #"-O3 -convert big_endian"
export RECURS=
export LDFLAGSM=${LDFLAGSM:-"-lifport -lifcoremt -lpthread"}
export OMPFLAGM=${OMPFLAGM:-""}

export INCSM="-I ${NEMSIO_INC} -I ${NETCDF_INCLUDES} -I ${cwd}/${progname}.cd"

export LIBSM="-Wl,-rpath,${NETCDF_LIBRARIES} ${NEMSIO_LIB} ${BACIO_LIB4} ${W3NCO_LIB4} ${NETCDF_LDFLAGS_C}"

make -f Makefile clobber
make -f Makefile
make -f Makefile install
make -f Makefile clobber

exit
