#!/bin/bash
set -eux

###############################################################################
#                                                                             #
# Compiles all codes, moves executables to exec and cleans up                 #
#                                                                             #
#                                                                 Dec, 2019   #
#                                                                             #
###############################################################################
#
# --------------------------------------------------------------------------- #

source ./machine-setup.sh
cwd=`pwd`

progname=wave_stat

if [ -f ../modulefiles/gefs/gefs_$target.ver ]; then
    source ../modulefiles/gefs/gefs_$target.ver
else
    if [ -f ../versions/build.ver ]; then
        source ../versions/build.ver
    fi
fi
source ../modulefiles/gefs/${progname}.$target

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

#
cd ${progname}.fd

export FCMP=${FCMP:-ifort}

make -f Makefile clobber
make -f Makefile
make -f Makefile install
make -f Makefile clobber

exit
