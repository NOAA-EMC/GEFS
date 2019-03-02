#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

progname=gettrk

source ../modulefiles/gefs/${progname}.$target             > /dev/null 2>&1


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

# If you want to get the original size of excutable file, 
GetOriginal=${GetOriginal:-false}

if $GetOriginal; then
    if [ $target == theia ]; then
        echo "This is on theia"
    elif [ $target == wcoss_cray ]; then
        echo "This is on wcoss_cray"
        echo "Please make sure you have modified the Makefile!"
        echo 'Comment the statement end with "# New and concise method"'
        echo '  and Uncomment the statement end with "# To get the original size of executable file"'
        export INCSM="-I ${SIGIO_INC4} -I/opt/cray/iobuf/2.0.5/include -I ${SFCIO_INC4} -I ${NEMSIOGFS_INC} -I ${NEMSIO_INC}"
    elif [ $target == wcoss ]; then
        echo "This is on wcoss"
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
