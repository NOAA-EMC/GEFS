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

# Load modulefile
  module purge
  module use ../modulefiles/gefs
  module load gefs_waves_sorc.module  
  module list

# 1. Preparations: seek source codes to be compiled

  fcodes=gwes_stats

  echo " FORTRAN codes found: "$fcodes

  outfile=`pwd`/make_gefswave_codes.out
  rm -f ${outfile}

# 2. Create executables

  for code in $fcodes 
  do
    echo " Making ${code} " >> ${outfile}
    cd ${code}.?d 
    make >> ${outfile}
    echo " Moving ${code} to exec" >> ${outfile}
    if [ ! -d ../../exec ]
      then
      mkdir -p ../../exec
    fi
    mv ${code} ../../exec
    echo " Cleaning up ${code} directory" >> ${outfile}
    make clean
    echo ' ' >> ${outfile}
    cd ..
  done
