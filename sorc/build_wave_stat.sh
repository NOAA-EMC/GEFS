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

make -f Makefile clobber
make -f Makefile
make -f Makefile install
make -f Makefile clobber

exit
