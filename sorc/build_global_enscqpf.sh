#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

progname=global_enscqpf

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

export FFLAGSM="-O3 -g -convert big_endian"
export RECURS=
export LDFLAGSM=${LDFLAGSM:-""}
export OMPFLAGM=${OMPFLAGM:-""}

export INCSM="-I ${G2_INC4}"

export LIBSM="${G2_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB}"

# If you want to get the original size of excutable file, 
GetOriginal=${GetOriginal:-false}

if $GetOriginal; then
    if [ $target == wcoss_cray ]; then
        echo "This is on wcoss_cray"
        echo "Please make sure you have modified the Makefile!"
        echo 'Comment the statement end with "# New and concise method"'
        echo '  and Uncomment the statement end with "# To get the original size of executable file"'
    elif [ $target == wcoss_dell_p3 ]; then
        echo "This is on wcoss_dell_p3"
        echo "Please make sure you have modified the Makefile!"
        echo 'Comment the statement end with "# New and concise method"'
        echo '  and Uncomment the statement end with "# To get the original size of executable file"'
    fi
fi

make -f Makefile clobber
make -f Makefile
make -f Makefile install
make -f Makefile clobber

exit
