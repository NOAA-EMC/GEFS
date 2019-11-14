#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

progname=gefs_anom2_fcst

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

#export FFLAGSM="-i4 -O3 -r8  -convert big_endian -fp-model precise"
export FFLAGSM="-O3 -g -convert big_endian"
export RECURS=
export LDFLAGSM=${LDFLAGSM:-""}
export OMPFLAGM=${OMPFLAGM:-""}

export INCSM="-I ${G2_INC4}"

export LIBSM="${W3NCO_LIB4} ${BACIO_LIB4}"

# If you want to get the original size of excutable file, 
GetOriginal=${GetOriginal:-false}

if $GetOriginal; then
    if [ $target == theia ]; then
        echo "This is on theia"
        echo "You don't need do anything, because it will not affect on Theia"
    elif [ $target == jet ]; then
        echo "This is on jet"
    elif [ $target == wcoss_cray ]; then
        echo "This is on wcoss_cray" 
        export INCSM="-I ${G2_INC4} -I/opt/cray/iobuf/2.0.5/include"
        export LIBSM="/gpfs/hps/nco/ops/nwprod/lib/g2/v2.5.0/intel/libg2_v2.5.0_4.a ${W3NCO_LIB4} ${BACIO_LIB4} /usrx/local/prod//jasper/1.900.1/gnu/haswell/lib/libjasper.a /usrx/local/prod//png/1.2.49/intel/haswell/lib/libpng.a /usrx/local/prod/zlib/1.2.7/intel/haswell/lib/libz.a"
    elif [ $target == wcoss ]; then
        echo "This is on wcoss"
        module load bufr/v11.2.0
        export G2_LIB4=/usrx/local/nceplibs/g2/v3.0.0/libg2_v3.0.0_4.a
        module load jasper/v1.900.1
        module load png/v1.2.44
        module load z/v1.2.6
        export LIBSM="${G2_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB}"
    elif [ $target == wcoss_dell_p3 ]; then
        echo "This is on wcoss_dell_p3"
    fi
fi


make -f Makefile clobber
make -f Makefile
make -f Makefile install
make -f Makefile clobber

exit
