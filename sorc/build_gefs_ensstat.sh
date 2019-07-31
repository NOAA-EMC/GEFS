#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

progname=gefs_ensstat

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

export FFLAGSM="-O3 -g -convert big_endian"
export RECURS=
export LDFLAGSM=${LDFLAGSM:-""}
export OMPFLAGM=${OMPFLAGM:-""}

export INCSM="-I ${G2_INC4}"

export LIBSM="${G2_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB}"

# If you want to get the original size of excutable file, 
GetOriginal=${GetOriginal:-false}

if $GetOriginal; then
    if [ $target == theia ]; then
        echo "This is on theia"
    elif [ $target == wcoss_cray ]; then
        echo "This is on wcoss_cray"
        export INCSM="-I ${G2_INC4} -I/opt/cray/iobuf/2.0.5/include"
        export LIBSM="${G2_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB} -Wl,/opt/cray/iobuf/2.0.5/lib/iobuf.o"
    elif [ $target == wcoss ]; then
        echo "This is on wcoss"
    elif [ $target == wcoss_dell_p3 ]; then
        echo "This is on wcoss_dell_p3"
    fi
fi


make -f Makefile clobber
make -f Makefile
make -f Makefile install
make -f Makefile clobber

exit
