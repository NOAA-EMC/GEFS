#!/bin/bash
###############################################################################
#                                                                             #
# Compiles all codes, moves executables to exec and cleans up                 #
#                                                                             #
#                                                                 Dec, 2019   #
#                                                                             #
###############################################################################
#
# --------------------------------------------------------------------------- #

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

progname=wave_stat

source ../modulefiles/gefs/${progname}.$target             > /dev/null 2>&1

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

#
#
cd ${progname}.fd

make -f Makefile clobber
make -f Makefile
make -f Makefile install
make -f Makefile clobber

exit