#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

progname=global_sigzvd

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

export FFLAGSM="-mkl -O2  -p -xHOST -r8  -convert big_endian -i4 -list -traceback -assume byterecl"
export RECURS=
export LDFLAGSM=${LDFLAGSM:-""}
export OMPFLAGM=${OMPFLAGM:-""}

export INCSM="-I ${SIGIO_INC4}"

export LIBSM="${NEMSIOGFS_LIB} ${NEMSIO_LIB} ${SP_LIBd} ${SIGIO_LIB4} ${W3NCO_LIBd} ${BACIO_LIB4}"
#export LIBSM="${SIGIO_LIB4}"

# If you want to get the original size of excutable file, 
GetOriginal=${GetOriginal:-false}

if $GetOriginal; then
    if [ $target == theia ]; then 
        echo "This is on theia"
    elif [ $target == wcoss_cray ]; then
        echo "This is on wcoss_cray" 
        export INCSM="-I ${SIGIO_INC4} -I/opt/cray/iobuf/2.0.5/include"
    elif [ $target == wcoss ]; then 
        echo "This is on wcoss"
        export FFLAGSM="-mkl -O2 -qopenmp -p -xHOST -r8  -convert big_endian -i4 -list -traceback -assume byterecl"
    elif [ $target == wcoss_dell_p3 ]; then
        echo "This is on wcoss_dell_p3"
        export FFLAGSM="-mkl -O2 -qopenmp -p -xHOST -r8  -convert big_endian -i4 -list -traceback -assume byterecl"
    fi  
fi

make -f Makefile clobber
make -f Makefile
make -f Makefile install
make -f Makefile clobber

exit
